module Main where

import Prelude

import CLI (window)
import Control.Monad.Error.Class (try)
import Data.Array (catMaybes, concat, dropEnd, filter, head, length, mapWithIndex, range, snoc, (!!))
import Data.Array as Array
import Data.DateTime (DateTime(..), adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Formatter.DateTime (Formatter, format)
import Data.Formatter.DateTime as Formatter
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Int as Integer
import Data.List as List
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Set (empty, fromFoldable, union)
import Data.Set as Set
import Data.String (joinWith, toUpper)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read)
import HTTPure (Method(..), Request, Response, ResponseM, ServerM, badRequest, created, header, internalServerError, notFound, ok, ok', serve, toString, (!@))
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Node.Process (argv)
import Options.Applicative (ParseError, briefDesc, execParser, helper, info, progDesc, (<**>))
import Parsing (parseErrorMessage, runParser)
import RDF (Quad, Term, Graph, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, value)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)

-- RDF Prefix for Stream Containers
ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data Window = Window Term Term Term Milliseconds Milliseconds

data StreamContainer = StreamContainer (Array Graph) (Array Window)

emptyStreamContainer :: StreamContainer
emptyStreamContainer = StreamContainer [] []

addGraphToStreamContainer :: Graph -> StreamContainer -> StreamContainer
addGraphToStreamContainer graph (StreamContainer graphArray windowArray) = StreamContainer (snoc graphArray graph) windowArray

getGraphFromStreamContainer :: Int -> StreamContainer -> Maybe Graph
getGraphFromStreamContainer i (StreamContainer graphArray _) = graphArray !! i

addWindowToStreamContainer :: Window -> StreamContainer -> StreamContainer
addWindowToStreamContainer window (StreamContainer graphArray windowArray) = StreamContainer graphArray (snoc windowArray $ window)

nextGraphId :: StreamContainer -> Int
nextGraphId (StreamContainer graphArray _) = length graphArray

getGraphsInWindow :: Array Graph -> DateTime -> Term -> Milliseconds -> Milliseconds -> Array Int
getGraphsInWindow graphArray now contentTimestampRelation startDuration endDuration = catMaybes $ mapWithIndex (\index graph -> booleanToMabyeIndex index $ isGraphInWindow now contentTimestampRelation startDuration endDuration graph) graphArray
  where
    booleanToMabyeIndex index graphInWindow = if graphInWindow then Just index else Nothing

isGraphInWindow :: DateTime -> Term -> Milliseconds -> Milliseconds -> Graph -> Boolean
isGraphInWindow now contentTimestampRelation startDuration endDuration graph = fromMaybe false do 
  quad <- Set.findMax $ Set.filter (\q -> predicate q == contentTimestampRelation) graph
  timestamp <- hush $ runParser (value $ object quad) parseDateTime
  windowStart <- adjust startDuration now
  windowEnd <- adjust endDuration now
  pure $ timestamp <= windowStart && timestamp >= windowEnd

membershipQuads :: DateTime -> Array Graph -> Window -> Array Quad
membershipQuads now graphArray (Window memberRelation membershipResource contentTimestampRelation startDuration endDuration) = map (\i -> quad membershipResource memberRelation (namedNode $ "/" <> show i) defaultGraph) $ getGraphsInWindow graphArray now contentTimestampRelation startDuration endDuration

streamContainerToQuads :: DateTime -> StreamContainer -> Array Quad
streamContainerToQuads now (StreamContainer graphArray windowArray) = [
  quad (namedNode "") (namedNode' rdf "type") (namedNode' ldpsc "StreamContainer") defaultGraph
] <>
  map (\i -> quad (namedNode "") (namedNode' ldp "contains") (namedNode $ "/" <> show i) defaultGraph) (dropEnd 1 (range 0 (length graphArray))) <>
  (concat $ mapWithIndex windowToQuads windowArray) <>
  (concat $ (membershipQuads now graphArray) <$> windowArray)

windowToQuads :: Int -> Window -> Array Quad
windowToQuads i (Window memberRelation membershipResource contentTimestampRelation startDuration endDuration) = [
  quad (namedNode "") (namedNode' ldpsc "window") window defaultGraph,
  quad window (namedNode' ldp "hasMemberRelation") memberRelation defaultGraph,
  quad window (namedNode' ldp "hasMembershipResource") membershipResource defaultGraph,
  quad window (namedNode' ldpsc "hasContentTimestampRelation") contentTimestampRelation defaultGraph,
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
  startDurationQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "startDuration") quads
  startDuration <- fromString $ value $ object startDurationQuad
  endDurationQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "endDuration") quads
  endDuration <- fromString $ value $ object endDurationQuad
  pure $ Window (object memberRelation) (object membershipResource) (object contentTimestampRelation) (Milliseconds startDuration) (Milliseconds endDuration)

formatForMIME :: Maybe String -> Format
formatForMIME (Just "text/turtle") = Turtle
formatForMIME (Just "application/trig") = TriG
formatForMIME (Just "application/n-triples") = NTriples
formatForMIME (Just "application/n-quads") = NQuads
formatForMIME Nothing = Turtle
formatForMIME _ = Turtle

getUnionGraph :: StreamContainer -> Graph
getUnionGraph (StreamContainer graphs _) = foldl union empty graphs

router :: Int -> Ref StreamContainer -> Request -> ResponseM
-- GET /
router port streamContainerRef request@{ method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let headerMaybe = lookup (CaseInsensitiveString "Accept-Datetime") headers :: Maybe String
  case headerMaybe of
    Nothing -> do
      time <- liftEffect nowDateTime
      createSCPayload streamContainer time
    Just timeString -> case runParser timeString parseDateTime of
        Left error -> do
          logWarn $ "Not able to parse Accept-Datetime of request: " <> show (parseErrorMessage error)
          logResponse request $ badRequest $ "Not able to parse Accept-Datetime of request: " <> show (parseErrorMessage error)
        Right time -> createSCPayload streamContainer time
  where
    createSCPayload streamContainer time = do
      payload <- try $ write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ streamContainerToQuads time streamContainer
      case payload of 
        Left error -> do
          logError $ "Serializing Turtle for Stream Container failed: " <> message error
          logResponse request $ internalServerError $ "Serializing Turtle for Stream Container failed: " <> message error
        Right body -> logResponse request $ ok' (header "Content-Type" "text/turtle") body
-- POST /
router port streamContainerRef request@{ method: Post, path: [], body, headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/" <> show (nextGraphId streamContainer)) (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  liftEffect $ modify_ (addGraphToStreamContainer $ fromFoldable payload) streamContainerRef
  logResponse request created
-- GET /all
router port streamContainerRef request@{ method: Get, path : [ "all" ], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let graph = getUnionGraph streamContainer
  payload <- write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ Array.fromFoldable graph
  logResponse request $ ok' (header "Content-Type" "text/turtle") payload
-- GET /{id}
router port streamContainerRef request@{ method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case Integer.fromString (path !@ 0) of 
  Nothing -> logResponse request notFound
  Just i -> case getGraphFromStreamContainer i streamContainer of 
    Nothing -> logResponse request notFound
    Just graph -> do
      payload <- write ("http://localhost:" <> show port <> "/" <> show i) (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ Array.fromFoldable graph
      logResponse request $ ok' (header "Content-Type" "text/turtle") payload
-- POST /window
router port streamContainerRef request@{ method: Post, path: [ "window" ], body, headers: (Headers headers) } = do
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  case addWindowToStreamContainer <$> quadsToWindow payload of 
    Nothing -> logResponse request $ badRequest "Could not parse window"
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
  options <-liftEffect $ execParser opts
  liftEffect $ log options
  args <- argv
  let port = fromMaybe 8080 do
        portString <- args !! 2
        Integer.fromString portString
  time <- liftEffect $ nowDateTime
  serve port (router port streamContainerRef) $ log $ "[INFO]\t[" <> format timeFormatter time <> "] Server up on port " <> show port
    where
    opts = info (window <**> helper)
      ( briefDesc
     <> progDesc "Start a Stream Container web server" )
