module Main
  ( StreamContainer(..)
  , addGraphToStreamContainer
  , addWindowToStreamContainer
  , emptyStreamContainer
  , formatForMIME
  , getGraphFromStreamContainer
  , getGraphsInWindow
  , getUnionGraph
  , isGraphInWindow
  , ldpsc
  , logDebug
  , logError
  , logInfo
  , logResponse
  , logWarn
  , main
  , membershipQuads
  , mimeForFormat
  , nextGraphId
  , quadsToWindow
  , router
  , streamContainerToQuads
  , timeFormatter
  , windowToQuads
  )
  where

import Prelude

import CLI (Window(..), optsInfo)
import Control.Monad.Error.Class (try)
import Data.Array (concat, dropWhile, filter, find, head, last, length, mapMaybe, mapWithIndex, snoc)
import Data.Array as Array
import Data.DateTime (DateTime, adjust, diff)
import Data.Either (Either(..), hush)
import Data.Foldable (foldl, foldr)
import Data.Formatter.DateTime (Formatter, format)
import Data.Formatter.DateTime as Formatter
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Int as Integer
import Data.List as List
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Set (Set, empty, fromFoldable, union)
import Data.Set as Set
import Data.String (joinWith, toUpper)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..), negateDuration)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read)
import HTTPure (Method(..), Request, Response, ResponseM, ServerM, badRequest, created, header, internalServerError, notFound, ok', serve, toString, (!@))
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Options.Applicative (execParser)
import Parsing (parseErrorMessage, runParser)
import RDF (Graph, Quad, Term, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, value)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)

-- RDF Prefix for Stream Containers
ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data StreamContainer = StreamContainer (Array (Tuple Int Graph)) (Array Window)

emptyStreamContainer :: StreamContainer
emptyStreamContainer = StreamContainer [] []

addGraphToStreamContainer :: DateTime -> Graph -> StreamContainer -> StreamContainer
addGraphToStreamContainer datetime newGraph (StreamContainer graphArray windowArray) = StreamContainer (snoc (dropWhile graphTooOld graphArray) (Tuple nextIdx newGraph)) windowArray
  where
    nextIdx :: Int
    nextIdx = fromMaybe 0 $ (add 1) <$> fst <$> last graphArray
    graphTooOld :: Tuple Int Graph -> Boolean
    graphTooOld graph = fromMaybe true do
        quad <- Set.findMax $ Set.filter (\q -> predicate q == namedNode "http://ex.org/vocab/timestamp") $ snd graph
        timestamp <- hush $ runParser (value $ object quad) parseDateTime
        tenSecondsBack <- adjust (negateDuration $ Milliseconds 10000.0) datetime
        pure $ timestamp < tenSecondsBack

getGraphFromStreamContainer :: Int -> StreamContainer -> Maybe (Tuple Int Graph)
getGraphFromStreamContainer i (StreamContainer graphArray _) = find (\(Tuple i2 _) -> i == i2) graphArray

addWindowToStreamContainer :: Window -> StreamContainer -> StreamContainer
addWindowToStreamContainer window (StreamContainer graphArray windowArray) = StreamContainer graphArray (snoc windowArray $ window)

nextGraphId :: StreamContainer -> Int
nextGraphId (StreamContainer graphArray _) = length graphArray

getGraphsInWindow :: Array (Tuple Int Graph) -> DateTime -> Term -> Milliseconds -> Milliseconds -> Array (Tuple Int Graph)
getGraphsInWindow graphArray now contentTimestampRelation startDuration endDuration = filter (\graph -> isGraphInWindow now contentTimestampRelation startDuration endDuration graph) graphArray

isGraphInWindow :: DateTime -> Term -> Milliseconds -> Milliseconds -> Tuple Int Graph -> Boolean
isGraphInWindow now contentTimestampRelation startDuration endDuration (Tuple _ graph) = fromMaybe false do 
  quad <- Set.findMax $ Set.filter (\q -> predicate q == contentTimestampRelation) graph
  timestamp <- hush $ runParser (value $ object quad) parseDateTime
  windowStart <- adjust startDuration now
  windowEnd <- adjust endDuration now
  pure $ timestamp <= windowStart && timestamp >= windowEnd

membershipQuads :: DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
membershipQuads now graphArray (Window memberRelation membershipResource contentTimestampRelation _ _ startDuration endDuration) = map (\(Tuple i _) -> quad membershipResource memberRelation (namedNode $ "/" <> show i) defaultGraph) $ getGraphsInWindow graphArray now contentTimestampRelation startDuration endDuration

poisonedQuads :: DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
poisonedQuads now graphArray (Window _ membershipResource contentTimestampRelation poisonRelation contentPoisonRelation startDuration endDuration) = if (Set.size $ Set.difference (allTimestampsInWindow (fromMaybe now $ adjust startDuration now) (fromMaybe now $ adjust endDuration now)) (Set.fromFoldable $ mapMaybe getPoisonedTimestamp inWindow)) == 0
  then
    [ quad membershipResource poisonRelation (literalType "true" (namedNode' xsd "boolean")) defaultGraph ]
  else 
    [ quad membershipResource poisonRelation (literalType "false" (namedNode' xsd "boolean")) defaultGraph ]
  where
    allTimestampsInWindow :: DateTime -> DateTime -> Set DateTime
    allTimestampsInWindow start end = if diff start end < Milliseconds 0.0 then Set.empty else Set.union (Set.singleton start) (allTimestampsInWindow (fromMaybe end $ adjust (negateDuration $ Seconds 1.0) start) end)
    inWindow :: Array (Tuple Int Graph)
    inWindow = filter (isGraphInWindow now contentTimestampRelation startDuration endDuration) graphArray
    getPoisonedTimestamp :: (Tuple Int Graph) -> Maybe DateTime 
    getPoisonedTimestamp graph = case find (\q -> predicate q == contentPoisonRelation && object q == literalType "true" (namedNode' xsd "boolean")) (Array.fromFoldable $ snd graph) of 
      Nothing -> Nothing
      Just _ -> do
        q <- find (\q -> predicate q == contentTimestampRelation) (Array.fromFoldable $ snd graph)
        hush $ runParser (value $ object q) parseDateTime

streamContainerToQuads :: DateTime -> StreamContainer -> Array Quad
streamContainerToQuads now (StreamContainer graphArray windowArray) = [
  quad (namedNode "") (namedNode' rdf "type") (namedNode' ldpsc "StreamContainer") defaultGraph
] <>
  map (\(Tuple i _) -> quad (namedNode "") (namedNode' ldp "contains") (namedNode $ "/" <> show i) defaultGraph) graphArray <>
  (concat $ mapWithIndex windowToQuads windowArray) <>
  (concat $ membershipQuads now graphArray <$> windowArray) <>
  (concat $ poisonedQuads now graphArray <$> windowArray)

windowToQuads :: Int -> Window -> Array Quad
windowToQuads i (Window memberRelation membershipResource contentTimestampRelation poisonRelation contentPoisonRelation startDuration endDuration) = [
  quad (namedNode "") (namedNode' ldpsc "window") window defaultGraph,
  quad window (namedNode' ldp "hasMemberRelation") memberRelation defaultGraph,
  quad window (namedNode' ldp "hasMembershipResource") membershipResource defaultGraph,
  quad window (namedNode' ldpsc "hasContentTimestampRelation") contentTimestampRelation defaultGraph,
  quad window (namedNode' ldpsc "hasPoisonRelation") poisonRelation defaultGraph,
  quad window (namedNode' ldpsc "hasContentPoisonRelation") contentPoisonRelation defaultGraph,
  quad window (namedNode' ldpsc "startDuration") (milisecondsToLiteral startDuration) defaultGraph,
  quad window (namedNode' ldpsc "endDuration") (milisecondsToLiteral endDuration) defaultGraph
]
  where
    window = blankNode $ "window-" <> show i
    milisecondsToLiteral (Milliseconds ms) = literalType (show ms) (namedNode' xsd "decimal")

quadsToWindow :: Array Quad -> Maybe Window
quadsToWindow quads = do
  memberRelation <- head $ filter (\quad -> predicate quad == namedNode' ldp "hasMemberRelation") quads
  membershipResource <- head $ filter (\quad -> predicate quad == namedNode' ldp "hasMembershipResource") quads
  contentTimestampRelation <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "hasContentTimestampRelation") quads
  poisonRelation <- head $ filter (\quad -> predicate quad == namedNode' ldp "hasPoisonRelation") quads
  contentPoisonRelation <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "hasContentPoisonRelation") quads
  startDurationQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "startDuration") quads
  startDuration <- fromString $ value $ object startDurationQuad
  endDurationQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "endDuration") quads
  endDuration <- fromString $ value $ object endDurationQuad
  pure $ Window (object memberRelation) (object membershipResource) (object contentTimestampRelation) (object poisonRelation) (object contentPoisonRelation) (Milliseconds startDuration) (Milliseconds endDuration)

formatForMIME :: String -> Format
formatForMIME "text/turtle" = Turtle
formatForMIME "application/trig" = TriG
formatForMIME "application/n-triples" = NTriples
formatForMIME "application/n-quads" = NQuads
formatForMIME _ = Turtle

mimeForFormat :: Format -> String
mimeForFormat Turtle = "text/turtle"
mimeForFormat TriG = "application/trig"
mimeForFormat NTriples = "application/n-triples"
mimeForFormat NQuads = "application/n-quads"

getUnionGraph :: StreamContainer -> Graph
getUnionGraph (StreamContainer graphs _) = foldl union empty $ snd <$> graphs

router :: Int -> Ref StreamContainer -> Request -> ResponseM
-- GET /
router port streamContainerRef request@{ method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  case lookup (CaseInsensitiveString "Accept-Datetime") headers of
    -- No Accept-Datetime header in the request, use now
    Nothing -> do
      time <- liftEffect nowDateTime
      createSCPayload streamContainer time
    Just timeString -> case runParser timeString parseDateTime of
        Left error -> do
          logWarn $ "Not able to parse Accept-Datetime of request: " <> parseErrorMessage error
          logResponse request $ badRequest $ "Not able to parse Accept-Datetime of request: " <> parseErrorMessage error
        Right time -> createSCPayload streamContainer time
  where
    createSCPayload streamContainer time = do
      -- serialize Triples for SC
      let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
      payload <- try $ write ("http://127.0.0.1:" <> show port <> "/") format $ streamContainerToQuads time streamContainer
      case payload of 
        Left error -> do
          logError $ "Serializing triples for Stream Container failed: " <> message error
          logResponse request $ internalServerError $ "Serializing Turtle for Stream Container failed: " <> message error
        Right body -> logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) body
-- POST /
router port streamContainerRef request@{ method: Post, path: [], body, headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  bodyString <- toString body
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- try $ parse ("http://127.0.0.1:" <> show port <> "/" <> show (nextGraphId streamContainer)) format bodyString
  case payload of 
    Left error -> do
      logError $ "Parsing request body failed: " <> message error
      logResponse request $ badRequest $ "Parsing request body failed: " <> message error
    Right quads -> do
      datetime <- liftEffect nowDateTime
      liftEffect $ modify_ (addGraphToStreamContainer datetime $ fromFoldable quads) streamContainerRef
      logResponse request created
-- GET /all
router port streamContainerRef request@{ method: Get, path : [ "all" ], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let graph = getUnionGraph streamContainer
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- write ("http://127.0.0.1:" <> show port <> "/") format $ Array.fromFoldable graph
  logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) payload
-- GET /{id}
router port streamContainerRef request@{ method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case Integer.fromString (path !@ 0) of 
  Nothing -> logResponse request notFound
  Just i -> case getGraphFromStreamContainer i streamContainer of 
    Nothing -> logResponse request notFound
    Just graph -> do
      let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
      payload <- try $ write ("http://127.0.0.1:" <> show port <> "/" <> show i) format $ Array.fromFoldable $ snd graph
      case payload of 
        Left error -> do
          logError $ "Serializing triples for contained resource failed: " <> message error
          logResponse request $ internalServerError $ "Serializing triples for contained resource failed: " <> message error
        Right rdf -> logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) rdf
-- POST /window
router port streamContainerRef request@{ method: Post, path: [ "window" ], body, headers: (Headers headers) } = do
  bodyString <- toString body
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- try $ parse ("http://127.0.0.1:" <> show port <> "/") format bodyString
  case payload of
    Left error -> do
      logError $ "Parsing request body failed: " <> message error
      logResponse request $ internalServerError $ "Parsing request body failed: " <> message error
    Right quads -> case addWindowToStreamContainer <$> quadsToWindow quads of 
      Nothing -> do
        logError $ "Triples in request body do not constitute a valid window"
        logResponse request $ badRequest "Triples in request body do not constitute a valid window"
      Just window -> do
        liftEffect $ modify_ window streamContainerRef
        logResponse request created
router _ _ request = logResponse request notFound

timeFormatter :: Formatter
timeFormatter = List.fromFoldable [
  Formatter.YearFull,
  Formatter.Placeholder "-",
  Formatter.MonthTwoDigits,
  Formatter.Placeholder "-",
  Formatter.DayOfMonthTwoDigits,
  Formatter.Placeholder " ",
  Formatter.Hours24,
  Formatter.Placeholder ":",
  Formatter.MinutesTwoDigits,
  Formatter.Placeholder ":",
  Formatter.SecondsTwoDigits,
  Formatter.Placeholder ".",
  Formatter.MillisecondsTwoDigits
]

logResponse :: forall m. MonadAff m => Request -> m Response -> m Response
logResponse { method, path } response = do
  { status } <- response
  logDebug $ toUpper (show method) <> " /" <> joinWith "/" path <> " " <> show status
  response

logDebug :: forall m. MonadAff m => String -> m Unit
logDebug message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[DEBUG]\t[" <> format timeFormatter time <> "] " <> message

logInfo :: forall m. MonadAff m => String -> m Unit
logInfo message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[INFO]\t[" <> format timeFormatter time <> "] " <> message

logWarn :: forall m. MonadAff m => String -> m Unit
logWarn message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[WARN]\t[" <> format timeFormatter time <> "] " <> message

logError :: forall m. MonadAff m => String -> m Unit
logError message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[ERROR]\t[" <> format timeFormatter time <> "] " <> message

main :: ServerM
main = do
  streamContainerRef <- new $ emptyStreamContainer
  opts <-liftEffect $ execParser optsInfo
  _ <- liftEffect $ modify_ (\sc -> foldr addWindowToStreamContainer sc opts.windows) streamContainerRef
  time <- liftEffect $ nowDateTime
  serve opts.port (router opts.port streamContainerRef) $ log $ "[INFO]\t[" <> format timeFormatter time <> "] Server up on port " <> show opts.port
