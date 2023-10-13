module Main
  ( StreamContainer(..)
  , _That
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
  , windowToQuads
  )
  where

import Prelude

import CLI (DataProvider(..), Experiment(..), Options, Window(..), optsInfo, uriOptions)
import Control.Monad.Error.Class (try)
import Data.Array (concat, dropWhile, filter, find, fold, head, init, last, length, mapMaybe, mapWithIndex, snoc, toUnfoldable, (!!))
import Data.Array as Array
import Data.DateTime (DateTime, Millisecond, adjust, diff, modifyTime, setMillisecond)
import Data.Either (Either(..), hush)
import Data.Enum (toEnum)
import Data.Foldable (foldl, foldr)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformatParser)
import Data.Formatter.DateTime as Formatter
import Data.Int (round, toNumber)
import Data.Int as Integer
import Data.Interval (Duration(..))
import Data.Lens (Prism', _Just, preview, prism', set)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString)
import Data.Posix.Signal (Signal(..))
import Data.Set (Set, empty, fromFoldable, union)
import Data.Set as Set
import Data.String (joinWith, toUpper)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.These (These(..))
import Data.Time.Duration (class Duration, Milliseconds(..), Seconds, negateDuration)
import Data.Time.Duration as DateTime
import Data.Time.Duration as Time
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import HTTPure (Method(..), Request, Response, ResponseM, ServerM, badRequest, created, header, internalServerError, notFound, ok', serve, toString, (!@))
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Node.Process (exit, onExit, onSignal)
import Options.Applicative (execParser)
import Parsing (parseErrorMessage, runParser)
import RDF (Graph, Quad, Term, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, value)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)
import URI (Path(..))
import URI.Path.Segment (segmentFromString)
import URI.Port (toInt)
import URI.URI (_authority, _hierPart, _hosts, _path)
import URI.URI as URI

iso8601Formatter :: Formatter
iso8601Formatter = List.fromFoldable
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
  , Placeholder "."
  , Formatter.Milliseconds
  , Placeholder "Z"
  ]

-- RDF Prefix for Stream Containers
ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data StreamContainer = StreamContainer (Array (Tuple Int Graph)) (Array Window)

emptyStreamContainer :: StreamContainer
emptyStreamContainer = StreamContainer [] []

addGraphToStreamContainer :: Options -> DateTime -> Graph -> StreamContainer -> StreamContainer
addGraphToStreamContainer opts datetime newGraph (StreamContainer graphArray windowArray) = StreamContainer (snoc (dropWhile graphTooOld graphArray) (Tuple nextIdx newGraph)) windowArray
  where
    nextIdx :: Int
    nextIdx = fromMaybe 0 $ (add 1) <$> fst <$> last graphArray
    largestWindow :: Seconds
    largestWindow = fromMaybe (Time.Seconds 0.0) $ Set.findMax $ Set.map (\(Window window) -> window.end) $ Set.fromFoldable windowArray
    graphTooOld :: Tuple Int Graph -> Boolean
    graphTooOld graph = fromMaybe true do
      quad <- Set.findMax $ Set.filter (\q -> predicate q == opts.contentTimestampRelation) $ snd graph
      timestamp <- hush $ runParser (value $ object quad) $ unformatParser (UnixTimestamp : Nil)
      largestWindowBack <- adjust (negateDuration largestWindow) datetime
      plusTenSecondsBack <- adjust (Time.Seconds (negate $ fromMaybe 200.0 $ fromString $ show opts.grace)) largestWindowBack
      pure $ timestamp < plusTenSecondsBack

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
  timestamp <- hush $ runParser (value $ object quad) $ unformatParser (UnixTimestamp : Nil)
  windowStart <- adjust (negateDuration start) now
  windowEnd <- adjust (negateDuration end) now
  pure $ timestamp <= windowStart && timestamp >= windowEnd

membershipQuads :: Options -> DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
membershipQuads opts now graphArray (Window window) = map (\(Tuple i _) -> quad window.membershipResource opts.memberRelation (namedNode $ "/" <> show i) defaultGraph) $ getGraphsInWindow graphArray now opts.contentTimestampRelation window.start window.end

poisonedQuads :: Options -> DateTime -> Array (Tuple Int Graph) -> Window -> Array Quad
poisonedQuads opts now graphArray (Window window) = if (Set.size $ Set.difference (allTimestampsInWindow (fromMaybe now $ adjust (negateDuration window.start) now) (fromMaybe now $ adjust (negateDuration window.end) now)) $ Set.fromFoldable $ mapMaybe getPoisonedTimestamp inWindow) == 0
  then
    [ quad window.membershipResource opts.poisonRelation (literalType "true" (namedNode' xsd "boolean")) defaultGraph ]
  else 
    [ quad window.membershipResource opts.poisonRelation (literalType "false" (namedNode' xsd "boolean")) defaultGraph ]
  where
    allTimestampsInWindow :: DateTime -> DateTime -> Set DateTime
    allTimestampsInWindow start end = if diff start end < Time.Seconds 0.0 then Set.empty else Set.union (Set.singleton start) (allTimestampsInWindow (fromMaybe start $ adjust (negateDuration $ Time.Seconds 1.0) start) end)
    inWindow :: Array (Tuple Int Graph)
    inWindow = filter (isGraphInWindow now opts.contentTimestampRelation window.start window.end) graphArray
    getPoisonedTimestamp :: (Tuple Int Graph) -> Maybe DateTime 
    getPoisonedTimestamp graph = case find (\q -> predicate q == opts.contentPoisonRelation && object q == literalType "true" (namedNode' xsd "boolean")) (Array.fromFoldable $ snd graph) of 
      Nothing -> Nothing
      Just _ -> do
        q <- find (\q -> predicate q == opts.contentTimestampRelation) (Array.fromFoldable $ snd graph)
        hush $ runParser (value $ object q) $ unformatParser (UnixTimestamp : Nil)

streamContainerToQuads :: Options -> DateTime -> StreamContainer -> Array Quad
streamContainerToQuads opts now (StreamContainer graphArray windowArray) = [
  quad (namedNode "") (namedNode' rdf "type") (namedNode' ldpsc "StreamContainer") defaultGraph,
  quad (namedNode "") (namedNode' ldp "hasMemberRelation") opts.memberRelation defaultGraph,
  quad (namedNode "") (namedNode' ldpsc "hasContentTimestampRelation") opts.contentTimestampRelation defaultGraph,
  quad (namedNode "") (namedNode' ldpsc "hasPoisonRelation") opts.poisonRelation defaultGraph,
  quad (namedNode "") (namedNode' ldpsc "hasContentPoisonRelation") opts.contentPoisonRelation defaultGraph,
  quad (namedNode "") (namedNode' ldpsc "currentTime") (literalType (format (UnixTimestamp : Nil) now) (namedNode' xsd "integer")) defaultGraph
] <> predQuads <>
  map (\(Tuple i _) -> quad (namedNode "") (namedNode' ldp "contains") (namedNode $ "/" <> show i) defaultGraph) graphArray <>
  (concat $ mapWithIndex windowToQuads windowArray) <>
  (concat $ membershipQuads opts now graphArray <$> windowArray) <>
  (concat $ poisonedQuads opts now graphArray <$> windowArray)
    where
      predQuads = case opts.predicate of 
        Just p -> [ quad (namedNode "") (namedNode' ldpsc "assignedPredicate") p defaultGraph ]
        Nothing -> []

windowToQuads :: Int -> Window -> Array Quad
windowToQuads i (Window window) = [
  quad (namedNode "") (namedNode' ldpsc "window") windowBN defaultGraph,
  quad windowBN (namedNode' ldp "hasMembershipResource") window.membershipResource defaultGraph,
  quad windowBN (namedNode' ldpsc "startDuration") (secondsToLiteral window.start) defaultGraph,
  quad windowBN (namedNode' ldpsc "endDuration") (secondsToLiteral window.end) defaultGraph
]
  where
    windowBN = blankNode $ "window-" <> show i
    secondsToLiteral (Time.Seconds s) = literalType (show $ round s) (namedNode' xsd "integer")

quadsToWindow :: Array Quad -> Maybe Window
quadsToWindow quads = do
  membershipResource <- object <$> (head $ filter (\quad -> predicate quad == namedNode' ldp "hasMembershipResource") quads)
  startQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "start") quads
  start <- Time.Seconds <$> (fromString $ value $ object startQuad)
  endQuad <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "end") quads
  end <- Time.Seconds <$> (fromString $ value $ object endQuad)
  pure $ Window { membershipResource, start, end }

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

router :: Options -> Ref Int -> Ref (Array DateTime) -> Ref StreamContainer -> Request -> ResponseM
-- GET /
router options _ _ streamContainerRef request@{ method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  case lookup (CaseInsensitiveString "Accept-Datetime") headers of
    -- No Accept-Datetime header in the request, u
    Nothing -> do
      time <- liftEffect nowDateTime
      createSCPayload streamContainer $ fromMaybe time $ (modifyTime <$> setMillisecond <$> toEnum 0) <*> Just time
    Just timeString -> case runParser timeString $ unformatParser (UnixTimestamp : Nil) of
        Left error -> do
          logWarn $ "Not able to parse Accept-Datetime of request: " <> parseErrorMessage error
          logResponse request $ badRequest $ "Not able to parse Accept-Datetime of request: " <> parseErrorMessage error
        Right time -> createSCPayload streamContainer $ fromMaybe time $ (modifyTime <$> setMillisecond <$> toEnum 0) <*> Just time
  where
    createSCPayload streamContainer time = do
      -- serialize Triples for SC
      let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
      payload <- try $ write (URI.print uriOptions options.uri) format $ streamContainerToQuads options time streamContainer
      case payload of 
        Left error -> do
          logError $ "Serializing triples for Stream Container failed: " <> message error
          logResponse request $ internalServerError $ "Serializing Turtle for Stream Container failed: " <> message error
        Right body -> logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) body
-- POST /
router options counterRef latenciesRef streamContainerRef request@{ method: Post, path: [], body, headers: (Headers headers) } = do
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
      liftEffect $ modify_ (\a -> a <> [datetime]) latenciesRef
      liftEffect $ modify_ (addGraphToStreamContainer options datetime $ fromFoldable quads) streamContainerRef
      liftEffect $ modify_ (\c -> c + 1) counterRef
      logResponse request created
-- GET /all
router options _ _ streamContainerRef request@{ method: Get, path : [ "all" ], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let graph = getUnionGraph streamContainer
  let format = formatForMIME $ fromMaybe "text/turtle" $ lookup (CaseInsensitiveString "Accept") headers
  payload <- write (URI.print uriOptions options.uri) format $ Array.fromFoldable graph
  logResponse request $ ok' (header "Content-Type" $ mimeForFormat format) payload
-- GET /{id}
router options _ _ streamContainerRef request@{ method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case Integer.fromString (path !@ 0) of 
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
router options _ _ streamContainerRef request@{ method: Post, path: [ "window" ], body, headers: (Headers headers) } = do
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
router _ _ _ _ request = logResponse request notFound

logResponse :: forall m. MonadAff m => Request -> m Response -> m Response
logResponse { method, path } response = do
  { status } <- response
  logDebug $ toUpper (show method) <> " /" <> joinWith "/" path <> " " <> show status
  response

logDebug :: forall m. MonadAff m => String -> m Unit
logDebug message = do
  time <- liftEffect $ nowDateTime
  --liftEffect $ log $ "[DEBUG]\t[" <> format (UnixTimestamp : Nil) time <> "] " <> message
  pure unit

logInfo :: forall m. MonadAff m => String -> m Unit
logInfo message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[INFO]\t[" <> format (UnixTimestamp : Nil) time <> "] " <> message

logWarn :: forall m. MonadAff m => String -> m Unit
logWarn message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[WARN]\t[" <> format (UnixTimestamp : Nil) time <> "] " <> message

logError :: forall m. MonadAff m => String -> m Unit
logError message = do
  time <- liftEffect $ nowDateTime
  liftEffect $ log $ "[ERROR]\t[" <> format (UnixTimestamp : Nil) time <> "] " <> message

_That :: forall a b. Prism' (These a b) b
_That = prism' That case _ of 
  This _ -> Nothing
  That a -> Just a
  Both _ a -> Just a

setupProvideData :: Options -> Ref Int -> Ref (Array DateTime) -> Ref StreamContainer -> Term -> Array DataProvider -> Effect Unit
setupProvideData opts counterRef latenciesRef scRef predicate dataProviders = do
  iRefsDataProviders <- sequence $ map iRefForDataProvider dataProviders
  _ <- setInterval opts.cycleTime $ provideData opts counterRef latenciesRef scRef predicate iRefsDataProviders
  pure unit
    where
      iRefForDataProvider :: DataProvider -> Effect (Tuple (Ref Int) DataProvider)
      iRefForDataProvider dp = do
        iRef <- new 0
        pure $ Tuple iRef dp

provideData :: Options -> Ref Int -> Ref (Array DateTime) -> Ref StreamContainer -> Term -> Array (Tuple (Ref Int) DataProvider) -> Effect Unit
provideData opts counterRef latenciesRef scRef predicate dataProviders = do
  fold <$> (sequence $ map (provideOneData false) $ fromMaybe [] $ init dataProviders)
  fromMaybe (pure unit) $ provideOneData true <$> last dataProviders
    where
      provideOneData :: Boolean -> Tuple (Ref Int) DataProvider -> Effect Unit
      provideOneData poison (Tuple iRef (DataProvider subject objects)) = do
        i <- read iRef
        now <- nowDateTime
        object <- case objects !! i of 
          Nothing -> do
            Ref.write 1 iRef
            pure $ fromMaybe (namedNode "error") $ head objects
          Just object -> do
            Ref.write (i + 1) iRef
            pure object
        let graph = Set.fromFoldable ([
          quad subject predicate object defaultGraph,
          quad (namedNode "") opts.contentTimestampRelation (literalType (format (UnixTimestamp : Nil) now) $ namedNode' xsd "integer") defaultGraph
        ] <> if poison then [ quad (namedNode "") opts.contentPoisonRelation (literalType "true" (namedNode' xsd "boolean")) defaultGraph ] else [])
        launchAff_ $ logDebug $ "Inserted '" <> show (quad subject predicate object defaultGraph)
        modify_ (addGraphToStreamContainer opts now graph) scRef
        modify_ (\c -> c + 1) counterRef
        modify_ (\a -> a <> [now]) latenciesRef

printThroughput :: Ref Int -> DateTime -> Effect Unit
printThroughput counterRef start = do
  now <- nowDateTime
  counter <- read counterRef
  let (DateTime.Milliseconds duration) = diff now start :: Milliseconds
  launchAff_ $ logInfo $ "Received " <> show counter <> " tokens in " <> show (duration/1000.0) <> " s. Average throughput is " <> show ((toNumber counter)/(duration/1000.0)) <> " tokens/s."
  exit 0

printLatency :: Ref (Array DateTime) -> Effect Unit
printLatency latenciesRef = do
  latencies <- read latenciesRef
  launchAff_ $ logInfo $ "Latencies " <> show (map (format iso8601Formatter) latencies)
  exit 0

main :: ServerM
main = do
  streamContainerRef <- new $ emptyStreamContainer
  counterRef <- new $ 0
  latenciesRef <- new $ []
  opts <- execParser optsInfo
  now <- nowDateTime
  case opts.experiment of 
    Nothing -> onSignal SIGTERM (exit 0)
    Just Throughput -> onSignal SIGTERM (printThroughput counterRef now)
    Just Latency -> onSignal SIGTERM (printLatency latenciesRef)
  _ <- setupProvideData opts counterRef latenciesRef streamContainerRef (fromMaybe (namedNode "error") opts.predicate) $ Array.fromFoldable opts.dataProviders
  _ <- modify_ (\sc -> foldr addWindowToStreamContainer sc opts.windows) streamContainerRef
  time <- nowDateTime
  serve (port opts) (router opts counterRef latenciesRef streamContainerRef) $ log $ "[INFO]\t[" <> format (UnixTimestamp : Nil) time <> "] Server up at " <> URI.print uriOptions opts.uri
    where
      port :: Options -> Int
      port opts = fromMaybe 8080 $ toInt <$> preview (_hierPart <<< _authority <<< _hosts <<< _Just <<< _That) opts.uri
