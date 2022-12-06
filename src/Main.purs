module Main where

import Prelude

import Affjax.Node (URL, defaultRequest, get, post, printError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (string)
import CLI (Options, Stream(..), Variables(..), optsInfo)
import Control.Parallel (parSequence, parSequence_)
import Data.Array (catMaybes, concat, concatMap, filter, find, fold, foldl, last, length, nub)
import Data.Array as Array
import Data.DateTime (DateTime, adjust, millisecond, time)
import Data.Either (Either(..), hush)
import Data.Enum (fromEnum)
import Data.Foldable (and, foldM)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.List (fromFoldable)
import Data.Map (Map, empty, insert, intersectionWith, lookup, singleton, union, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup.Foldable (foldl1)
import Data.String (Pattern(..), contains, joinWith, split, toUpper)
import Data.Time.Duration (negateDuration)
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import N3 (Format(..), parse, write)
import Options.Applicative (execParser)
import Parsing (runParser)
import RDF (Quad, Term, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, subject, termType, value)
import RDF.Prefixes (Prefix(..), rdf, xsd)

--data Triple = Triple TermOrVariable TermOrVariable TermOrVariable
--instance showTriple :: Show Triple where
--  show (Triple p s o) = show p <> "(" <> show s <> "," <> show o <> ")"

--data Aggregation = MMax | MMin | MSum | MCount | MAvg
--instance showAggregation :: Show Aggregation where
--  show MMax = "mmax"
--  show MMin = "mmin"
--  show MSum = "msum"
--  show MCount = "mcount"
--  show MAvg = "mavg"

--data Rule = Diamond Triple Triple | Box Triple Triple | And Triple (Array Triple)-- | Agg Aggregation Triple TermOrVariable Triple
--instance showRule :: Show Rule where
--  show (Diamond head body) = show head <> " ⟵ " <> "◇" <> show body
--  show (Box head body) = show head <> " ⟵ " <> "□" <> show body
--  show (And head triples) = show head <> " ⟵ " <> (joinWith " ∧ " $ map show triples)
--  show (Agg aggregation head result body) = show head <> " ⟵ " <> show result <> " = " <> show body

--rule :: Rule
----rule = Box (Triple (NoVariable $ namedNode "http://example.org/blinkedRightLast3Step") (Variable "car") (Variable "value")) (Triple (NoVariable $ namedNode "http://example.org/properties/blinkerRight") (Variable "car") (Variable "value"))
--rule = Agg MCount (
--    Triple
--    (NoVariable $ namedNode "blinkedRightCount")
--    (Variable "CAR")
--    (Variable "M")
--  ) (Variable "M") (
--    Triple 
--    (NoVariable $ namedNode "http://example.org/properties/blinkerRight")
--    (Variable "CAR")
--    (NoVariable $ literalType "true" $ namedNode' xsd "boolean")
--  )

data TermOrVariable = NoVariable Term | Variable String
derive instance eqTermOrVariable :: Eq TermOrVariable
derive instance ordTermOrVariable :: Ord TermOrVariable
instance showTermOrVariable :: Show TermOrVariable where
  show (NoVariable term) = fromMaybe (show term) $ getSuffix term
  show (Variable variable) = toUpper variable

data Fact = Fact Term Term Term DateTime
derive instance eqFact :: Eq Fact
derive instance ordFact :: Ord Fact
instance showFact :: Show Fact where
  show (Fact predicate subject object time) = p <> "(" <> s <> "," <> o <> ")@" <> t <> "."
    where
      p = (fromMaybe (show predicate) $ getSuffix predicate)
      s = (fromMaybe (show subject) $ getSuffix subject)
      o = (fromMaybe (show object) $ getSuffix object)
      t = format iso8601Formatter time

--type Binding = Map TermOrVariable Term
--
--getBinding :: Fact -> Triple -> Maybe Binding
--getBinding (Fact p s o _) (Triple p' s' o') = do
--  pBind <- bindTerm p p'
--  sBind <- bindTerm s s'
--  oBind <- bindTerm o o'
--  pure $ union pBind $ union sBind oBind
--
--applyBinding :: DateTime -> Binding -> Triple -> Maybe Fact
--applyBinding now binding (Triple p s o) = do
--  p' <- replaceVariable binding p
--  s' <- replaceVariable binding s
--  o' <- replaceVariable binding o
--  pure $ Fact p' s' o' now
--
--replaceVariable :: Binding -> TermOrVariable -> Maybe Term
--replaceVariable _ (NoVariable term) = Just term
--replaceVariable binding var = lookup var binding
--
--bindTerm :: Term -> TermOrVariable -> Maybe Binding
--bindTerm term (NoVariable term') = if term == term' then Just empty else Nothing
--bindTerm term var = Just $ singleton var term
--
--applyRule :: DateTime -> Rule -> Array Fact -> Array Fact
--applyRule now (Diamond head body) facts = nub $ catMaybes $ map (\b -> applyBinding now b head) bindings
--  where
--    bindings = catMaybes $ map (\fact -> getBinding fact body) facts
--applyRule now (Box head body) facts = if length bindings == length facts && length (nub bindings) == 1 then nub $ catMaybes $ map (\b -> applyBinding now b head) bindings else []
--  where
--    bindings = catMaybes $ map (\fact -> getBinding fact body) facts
--applyRule now (And head triples) facts = if length bindings == length facts then nub $ catMaybes [ applyBinding now (foldl union empty bindings) head ] else []
--  where
--    bindings = catMaybes $ concat $ map (\fact -> map (\triple -> getBinding fact triple) triples) facts
----applyRule now (Agg MCount head result body) facts = trace body \_ -> trace facts \_ -> nub $ catMaybes $ map (\b -> applyBinding now b head) $ map (insert result value) bindings
----  where
----    bindings = catMaybes $ map (\fact -> getBinding fact body) facts
----    value = literalType (show $ length bindings) $ namedNode' xsd "integer"
--applyRule _ _ _ = []

sosa :: Prefix
sosa = Prefix "http://www.w3.org/ns/sosa/"

getSuffix :: Term -> Maybe String
getSuffix namedNode | termType namedNode == "NamedNode" = if contains (Pattern "#") (value namedNode) then last (split (Pattern "#") $ value namedNode) else  last (split (Pattern "/") $ value namedNode)
getSuffix literal | termType literal == "Literal" = Just $ value literal
getSuffix _ = Nothing

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

membershipResource :: Term
membershipResource = namedNode "http://localhost:8082/#window"

memberRelation :: Term
memberRelation = namedNode "http://localhost:8082/#inWindow"

main :: Effect Unit
main = do
  opts <-liftEffect $ execParser optsInfo
  liftEffect $ logShow opts
--  liftEffect $ log $ "Rule: " <> show rule
  _ <- setInterval 1000 $ loop opts
  pure unit

loop :: Options -> Effect Unit
loop opts = launchAff_ do
  dateTime <- liftEffect $ nowDateTime
  let roundedDateTime = fromMaybe dateTime $ adjust (negateDuration $ (\ms -> Duration.Milliseconds ms) $ toNumber $ fromEnum $ millisecond $ time dateTime) dateTime
  liftEffect $ log $ "Time: " <> format iso8601Formatter roundedDateTime
  bindings <- parSequence $ map (getBindingsForSource roundedDateTime) opts.sources
  --let combinedBinding = fold combineBindings $ Array.fromFoldable bindings
  liftEffect $ logShow bindings
  --liftEffect $ logShow combinedBinding

type Binding = Map String Term

getBindingsForSource :: DateTime -> Stream -> Aff (Array Binding)
getBindingsForSource dateTime (Stream uri pred variables) = do
  containerQuads <- getQuads dateTime uri
  let obsInWindow = map (\q -> value $ object q) $ filter (\q -> subject q == namedNode uri && predicate q == namedNode "http://ex.org/vocab/inWindow") containerQuads
  obsQuadArrays <- parSequence $ getQuads dateTime <$> obsInWindow
  case variables of 
    Unary var -> do
      let obsQuads = concatMap (\qs -> filter (\q -> object q == namedNode pred && predicate q == namedNode' rdf "type" ) qs) obsQuadArrays
      pure $ map (\q -> Map.singleton var (subject q)) obsQuads
    Binary var1 var2 -> do
      let obsQuads = concatMap (\qs -> filter (\q -> predicate q == namedNode pred) qs) obsQuadArrays
      pure $ Map.fromFoldable <$> map (\q -> [ Tuple var1 $ subject q, Tuple var2 $ object q ]) obsQuads

--combineBindings :: Array Binding -> Array Binding -> Array (Maybe Binding)
--combineBindings bs1 bs2 = do
--  b1 <- bs1
--  b2 <- bs2

--bind :: Binding -> Stream -> Aff Unit
--bind binding (Stream uri variables) = do

datalogFromQuads :: Array Quad -> Maybe Fact
datalogFromQuads quads = do
  p <- object <$> find (\q -> predicate q == namedNode' sosa "observedProperty") quads
  s <- object <$> find (\q -> predicate q == namedNode' sosa "hasFeatureOfInterest") quads
  o <- object <$> find (\q -> predicate q == namedNode' sosa "hasSimpleResult") quads
  t <- join $ (\dateTimeString -> hush $ runParser dateTimeString parseDateTime) <$> value <$> object <$> find (\q -> predicate q == namedNode' sosa "resultTime") quads
  pure $ Fact p s o t

quadsFromDatalog :: Fact -> Array Quad
quadsFromDatalog (Fact p s o t) = [
  quad (namedNode "") (namedNode' rdf "type") (namedNode' sosa "Observation") defaultGraph,
  quad (namedNode "") (namedNode' sosa "resultTime") (literalType (format iso8601Formatter t) (namedNode' xsd "dateTimeStamp")) defaultGraph,
  quad (namedNode "") (namedNode' sosa "hasSimpleResult") o defaultGraph,
  quad (namedNode "") (namedNode' sosa "hasFeatureOfInterest") s defaultGraph,
  quad (namedNode "") (namedNode' sosa "observedProperty") p defaultGraph
]

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