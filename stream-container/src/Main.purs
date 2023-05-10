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

import CLI (Window(..), Options, optsInfo, uriOptions)
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
import Data.Lens (Prism', _Just, preview, prism', set)
import Data.List as List
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Set (Set, empty, fromFoldable, union)
import Data.Set as Set
import Data.String (joinWith, toUpper)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.These (These(..))
import Data.Time.Duration (Seconds(..), negateDuration)
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
import URI (Path(..))
import URI.Path.Segment (segmentFromString)
import URI.Port (toInt)
import URI.URI (_authority, _hierPart, _hosts, _path)
import URI.URI as URI

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
        tenSecondsBack <- adjust (negateDuration $ Seconds 10.0) datetime
        pure $ timestamp < tenSecondsBack

getGraphFromStreamContainer :: Int -> StreamContainer -> Maybe (Tuple Int Graph)
getGraphFromStreamContainer i (StreamContainer graphArray _) = find (\(Tuple i2 _) -> i == i2) graphArray

addWindowToStreamContainer :: Window -> StreamContainer -> StreamContainer
addWindowToStreamContainer window (StreamContainer graphArray windowArray) = StreamContainer graphArray (snoc windowArray $ window)

nextGraphId :: StreamContainer -> Int
nextGraphId (StreamContainer graphArray _) = length graphArray

getGraphsInWindow :: Array (Tuple Int Graph) -> DateTime -> Term -> Seconds -> Seconds -> Array (Tuple Int Graph)
getGraphsInWindow graphArray now contentTimestampRelation start end = filter (\graph -> isGraphInWindow now contentTimestampRelation start end graph) graphArray

isGraphInWindow :: DateTime -> Term -> Seconds -> Seconds -> Tuple Int Graph -> Boolean
isGraphInWindow now contentTimestampRelation start end (Tuple _ graph) = fromMaybe false do 
  quad <- Set.findMax $ Set.filter (\q -> predicate q == contentTimestampRelation) graph
  timestamp <- hush $ runParser (value $ object quad) parseDateTime
  windowStart <- adjust start now
  windowEnd <- adjust end now
  pure $ timestamp <= windowStart && timestamp >= windowEnd

membershipQuads :: DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
membershipQuads now graphArray (Window window) = map (\(Tuple i _) -> quad window.membershipResource window.hasMemberRelation (namedNode $ "/" <> show i) defaultGraph) $ getGraphsInWindow graphArray now window.contentTimestampRelation window.start window.end

poisonedQuads :: DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
poisonedQuads now graphArray (Window window) = if (Set.size $ Set.difference (allTimestampsInWindow (fromMaybe now $ adjust window.start now) (fromMaybe now $ adjust window.end now)) (Set.fromFoldable $ mapMaybe getPoisonedTimestamp inWindow)) == 0
  then
    [ quad window.membershipResource window.hasPoisonRelation (literalType "true" (namedNode' xsd "boolean")) defaultGraph ]
  else 
    [ quad window.membershipResource window.hasPoisonRelation (literalType "false" (namedNode' xsd "boolean")) defaultGraph ]
  where
    allTimestampsInWindow :: DateTime -> DateTime -> Set DateTime
    allTimestampsInWindow start end = if diff start end < Seconds 0.0 then Set.empty else Set.union (Set.singleton start) (allTimestampsInWindow (fromMaybe end $ adjust (negateDuration $ Seconds 1.0) start) end)
    inWindow :: Array (Tuple Int Graph)
    inWindow = filter (isGraphInWindow now window.contentTimestampRelation window.start window.end) graphArray
    getPoisonedTimestamp :: (Tuple Int Graph) -> Maybe DateTime 
    getPoisonedTimestamp graph = case find (\q -> predicate q == window.contentPoisonRelation && object q == literalType "true" (namedNode' xsd "boolean")) (Array.fromFoldable $ snd graph) of 
      Nothing -> Nothing
      Just _ -> do
        q <- find (\q -> predicate q == window.contentTimestampRelation) (Array.fromFoldable $ snd graph)
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
windowToQuads i (Window window) = [
  quad (namedNode "") (namedNode' ldpsc "window") windowBN defaultGraph,
  quad windowBN (namedNode' ldp "hasMemberRelation") window.hasMemberRelation defaultGraph,
  quad windowBN (namedNode' ldp "hasMembershipResource") window.membershipResource defaultGraph,
  quad windowBN (namedNode' ldpsc "hasContentTimestampRelation") window.contentTimestampRelation defaultGraph,
  quad windowBN (namedNode' ldpsc "hasPoisonRelation") window.hasPoisonRelation defaultGraph,
  quad windowBN (namedNode' ldpsc "hasContentPoisonRelation") window.contentPoisonRelation defaultGraph,
  quad windowBN (namedNode' ldpsc "startDuration") (secondsToLiteral window.start) defaultGraph,
  quad windowBN (namedNode' ldpsc "endDuration") (secondsToLiteral window.end) defaultGraph
]
  where
    windowBN = blankNode $ "window-" <> show i
    secondsToLiteral (Seconds s) = literalType (show s) (namedNode' xsd "integer")

quadsToWindow :: Array Quad -> Maybe Window
quadsToWindow quads = do
  membershipResource <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldp "hasMembershipResource") quads)
  hasMemberRelation <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldp "hasMemberRelation") quads)
  contentTimestampRelation <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldpsc "hasContentTimestampRelation") quads)
  hasPoisonRelation <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldp "hasPoisonRelation") quads)
  contentPoisonRelation <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldpsc "hasContentPoisonRelation") quads)
  startQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "start") quads
  start <- Seconds <$> (fromString $ value $ object startQuad)
  endQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "end") quads
  end <- Seconds <$> (fromString $ value $ object endQuad)
  pure $ Window { membershipResource, hasMemberRelation, contentTimestampRelation, hasPoisonRelation, contentPoisonRelation, start, end }

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

router :: Options -> Ref StreamContainer -> Request -> ResponseM
-- GET /
router options streamContainerRef request@{ method: Get, path: [], headers: (Headers headers) } = do
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
      payload <- try $ write (URI.print uriOptions options.uri) format $ streamContainerToQuads time streamContainer
      case payload of 
        Left error -> do
          logError $ "Serializing triples for Stream Container failed: " <> message error
          logResponse request $ internalServerError $ "Serializing Turtle for Stream Container failed: " <> message error
        Right body -> logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) body
-- POST /
router options streamContainerRef request@{ method: Post, path: [], body, headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  bodyString <- toString body
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- try $ parse (URI.print uriOptions $ set (_hierPart <<< _path) (Path [ segmentFromString $ show $ nextGraphId streamContainer ]) options.uri) format bodyString
  case payload of 
    Left error -> do
      logError $ "Parsing request body failed: " <> message error
      logResponse request $ badRequest $ "Parsing request body failed: " <> message error
    Right quads -> do
      datetime <- liftEffect nowDateTime
      liftEffect $ modify_ (addGraphToStreamContainer datetime $ fromFoldable quads) streamContainerRef
      logResponse request created
-- GET /all
router options streamContainerRef request@{ method: Get, path : [ "all" ], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let graph = getUnionGraph streamContainer
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- write (URI.print uriOptions options.uri) format $ Array.fromFoldable graph
  logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) payload
-- GET /{id}
router options streamContainerRef request@{ method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case Integer.fromString (path !@ 0) of 
  Nothing -> logResponse request notFound
  Just i -> case getGraphFromStreamContainer i streamContainer of 
    Nothing -> logResponse request notFound
    Just graph -> do
      let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
      payload <- try $ write (URI.print uriOptions $ set (_hierPart <<< _path) (Path [ segmentFromString $ show i ]) options.uri) format $ Array.fromFoldable $ snd graph
      case payload of 
        Left error -> do
          logError $ "Serializing triples for contained resource failed: " <> message error
          logResponse request $ internalServerError $ "Serializing triples for contained resource failed: " <> message error
        Right rdf -> logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) rdf
-- POST /window
router options streamContainerRef request@{ method: Post, path: [ "window" ], body, headers: (Headers headers) } = do
  bodyString <- toString body
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- try $ parse (URI.print uriOptions options.uri) format bodyString
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

_That :: forall a b. Prism' (These a b) b
_That = prism' That case _ of 
  This _ -> Nothing
  That a -> Just a
  Both _ a -> Just a

main :: ServerM
main = do
  streamContainerRef <- new $ emptyStreamContainer
  opts <-liftEffect $ execParser optsInfo
  _ <- liftEffect $ modify_ (\sc -> foldr addWindowToStreamContainer sc opts.windows) streamContainerRef
  time <- liftEffect $ nowDateTime
  serve (port opts) (router opts streamContainerRef) $ log $ "[INFO]\t[" <> format timeFormatter time <> "] Server up at " <> URI.print uriOptions opts.uri
    where
      port :: Options -> Int
      port opts = fromMaybe 8080 $ toInt <$> preview (_hierPart <<< _authority <<< _hosts <<< _Just <<< _That) opts.uri
