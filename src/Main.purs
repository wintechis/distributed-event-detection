module Main where

import Prelude

import Affjax.Node (URL, defaultRequest, post, printError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (string)
import CLI (Builtin(..), Options, Stream(..), Terms(..), optsInfo)
import Control.Alternative (guard)
import Control.Parallel (parSequence)
import Data.Array (catMaybes, concatMap, filter, findIndex, foldl, index, length, mapMaybe, mapWithIndex, (!!))
import Data.Array as Array
import Data.Array.NonEmpty (foldl1)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime (DateTime, adjust, millisecond, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (and)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Set (Set, member)
import Data.Set as Set
import Data.String (joinWith)
import Data.Time.Duration (negateDuration)
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import N3 (Format(..), parse, write)
import Options.Applicative (execParser)
import RDF (Quad, Term, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, subject, termType, value)
import RDF.Prefixes (rdf, xsd)

iso8601Formatter :: Formatter
iso8601Formatter = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder "Z"
  ]

filterBuiltin :: Relation -> Builtin -> Relation
filterBuiltin (Relation (RelationHeader header) rows) (LessThanEqual term1 term2) = Relation (RelationHeader header) $ mapMaybe filterRow rows
  where
    value1 :: RelationRow -> Maybe Number
    value1 (RelationRow row) = case termType term1 of
      "Literal" -> fromString $ value term1
      "Variable" -> do
        i <- findIndex (\h -> h == term1) header :: Maybe Int
        t <- index row i
        fromString $ value t
      _ -> Nothing
    value2 :: RelationRow -> Maybe Number
    value2 (RelationRow row) = case termType term2 of
      "Literal" -> fromString $ value term2
      "Variable" -> do
        i <- findIndex (\h -> h == term2) header :: Maybe Int
        t <- index row i
        fromString $ value t
      _ -> Nothing
    filterRow :: RelationRow -> Maybe RelationRow
    filterRow row = if value1 row <= value2 row then Just row else Nothing

newtype RelationRow = RelationRow (Array Term)
instance showRelationRow :: Show RelationRow where
  show (RelationRow row) = "(" <> (joinWith ", " $ map show row) <> ")\n"
newtype RelationHeader = RelationHeader (Array Term)
instance showRelationHeader :: Show RelationHeader where
  show (RelationHeader header) = (joinWith ", " $ map show header) <> "\n"
data Relation = Relation RelationHeader (Array RelationRow)
instance showRelation :: Show Relation where
  show (Relation header rows) = show header <> (joinWith "" $ map show rows)

newRelation :: RelationHeader -> Relation
newRelation header = Relation header []

addRow :: RelationRow -> Relation -> Relation
addRow newRow (Relation header rows) = Relation header $ rows <> [ newRow ]

naturalJoin :: Relation -> Relation -> Relation
naturalJoin (Relation (RelationHeader header1) rows1) (Relation (RelationHeader header2) rows2) = Relation combinedHeaders $ catMaybes $ joinRow <$> rows1 <*> rows2
  where
    joinRow :: RelationRow -> RelationRow -> Maybe RelationRow
    joinRow (RelationRow row1) (RelationRow row2) = if and $ map (\(Tuple i1 i2) -> row1 !! i1 == row2 !! i2) headerMatches
      then Just $ RelationRow $ row1 <> catMaybes (mapWithIndex (\i v -> if member i header2ForRemoval then Nothing else Just v) row2 )
      else Nothing
    headerMatches :: Array (Tuple Int Int)
    headerMatches = do
      Tuple i1 h1 <- mapWithIndex (\i h -> Tuple i h) header1
      Tuple i2 h2 <- mapWithIndex (\i h -> Tuple i h) header2
      guard $ h1 == h2
      pure $ Tuple i1 i2
    header2ForRemoval :: Set Int
    header2ForRemoval = Set.fromFoldable $ map snd headerMatches
    combinedHeaders :: RelationHeader
    combinedHeaders = RelationHeader $ header1 <> catMaybes (mapWithIndex (\i v -> if member i header2ForRemoval then Nothing else Just v) header2 )


getRelationForSource :: DateTime -> Stream -> Aff Relation
getRelationForSource dateTime (Stream uri pred variables) = do
  containerQuads <- getQuads dateTime uri
  let obsInWindow = map (\q -> value $ object q) $ filter (\q -> subject q == namedNode uri && predicate q == namedNode "http://ex.org/vocab/inWindow") containerQuads
  obsQuadArrays <- parSequence $ getQuads dateTime <$> obsInWindow :: Aff (Array (Array Quad))
  case variables of 
    Unary var -> do
      let obsQuads = concatMap (\qs -> filter (\q -> object q == pred && predicate q == namedNode' rdf "type" ) qs) obsQuadArrays
      let relation = newRelation (RelationHeader [ var ])
      pure $ foldl (\r q -> addRow (RelationRow [ subject q ]) r) relation obsQuads
    Binary var1 var2 -> do
      let obsQuads = concatMap (\qs -> filter (\q -> predicate q == pred) qs) obsQuadArrays
      let relation = newRelation (RelationHeader [ var1, var2 ])
      pure $ foldl (\r q -> addRow (RelationRow [ subject q, object q ]) r) relation obsQuads

postRelationToGoal :: DateTime -> Stream -> Relation -> Aff Unit
postRelationToGoal dateTime (Stream uri pred variables) (Relation (RelationHeader header) rows) = do
  case variables of
    Unary var -> if length payload > 0 then postQuads (payload <> [ quad (namedNode "") (namedNode "http://ex.org/vocab/timestamp") (literalType (format iso8601Formatter dateTime) (namedNode' xsd "dateTime")) defaultGraph ]) uri else pure unit
      where
        payload :: Array Quad
        payload = map (\t -> quad t (namedNode' rdf "type") pred defaultGraph) $ mapMaybe (getTermFromRelationRow var) rows
    Binary var1 var2 -> if length payload > 0 then postQuads (payload <> [ quad (namedNode "") (namedNode "http://ex.org/vocab/timestamp") (literalType (format iso8601Formatter dateTime) (namedNode' xsd "dateTime")) defaultGraph ]) uri else pure unit
      where
        payload :: Array Quad
        payload = map (\(Tuple t1 t2) -> quad t1 pred t2 defaultGraph) $ mapMaybe (\r -> Tuple <$> (getTermFromRelationRow var1 r) <*> (getTermFromRelationRow var2 r)) rows
    where
      getTermFromRelationRow :: Term -> RelationRow -> Maybe Term
      getTermFromRelationRow head (RelationRow row) = do
        i <- findIndex (\h -> h == head) header
        row !! i

getQuads :: DateTime -> URL -> Aff (Array Quad)
getQuads acceptDateTime url = do
  liftEffect $ log ("GET " <> url)
  responseOrError <- request (defaultRequest { url = url, method = Left GET, responseFormat = string, headers = [ RequestHeader "Accept-Datetime" $ format iso8601Formatter acceptDateTime ] })
  case responseOrError of 
    Left error -> do
      liftEffect $ log $ printError error
      pure []
    Right response -> parse url Turtle  response.body

postQuads :: Array Quad -> URL -> Aff Unit
postQuads quads url = do
  liftEffect $ log ("POST " <> url)
  payload <- write url Turtle quads
  responseOrError <- post string url (Just $ String payload)
  case responseOrError of 
    Left error -> do
      liftEffect $ log $ printError error
      pure unit
    Right _ -> pure unit

main :: Effect Unit
main = do
  opts <-execParser optsInfo
  logShow opts
  void $ setInterval 1000 $ loop opts

loop :: Options -> Effect Unit
loop opts = launchAff_ do
  dateTime <- liftEffect $ nowDateTime
  -- TODO Remove + 1
  let roundedDateTime = fromMaybe dateTime $ adjust (negateDuration $ (\ms -> Duration.Milliseconds ms) $ toNumber $ (\i -> i + 1) $ fromEnum $ millisecond $ time dateTime) dateTime
  liftEffect $ log $ "Time: " <> format iso8601Formatter roundedDateTime
  relations <- parSequence $ map (getRelationForSource roundedDateTime) (Array.fromFoldable opts.sources) :: Aff (Array Relation)
  let joined = fromMaybe (newRelation (RelationHeader [])) $ (foldl1 naturalJoin) <$> (NonEmpty.fromArray relations)
  let builtinJoined = foldl filterBuiltin joined $ Array.fromFoldable opts.builtins
  postRelationToGoal roundedDateTime opts.goal builtinJoined
  liftEffect $ logShow builtinJoined
