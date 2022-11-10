module Main where

import Prelude

import Data.Array (catMaybes, concat, dropEnd, filter, head, length, mapWithIndex, range, snoc, (!!))
import Data.Array as Array
import Data.DateTime (DateTime, adjust)
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Formatter.Parser.Interval (parseDateTime)
import Data.Int as Integer
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Set (empty, fromFoldable, union)
import Data.Set as Set
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read)
import HTTPure (Method(..), Request, ResponseM, ServerM, badRequest, created, header, notFound, ok', serve, toString, (!@))
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Node.Process (argv)
import Parsing (runParser)
import RDF (Quad, Term, Graph, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, value)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)

ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data StreamContainer = StreamContainer (Array Graph) (Array Window)

data Window = Window Term Term Term Milliseconds Milliseconds

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
router port streamContainerRef { method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  now <- liftEffect $ nowDateTime
  let time = fromMaybe now $ do
        timeString <- lookup (CaseInsensitiveString "Accept-Datetime") headers
        hush $ runParser timeString parseDateTime
  payload <- write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ streamContainerToQuads time streamContainer
  ok' (header "Content-Type" "text/turtle") payload
-- POST /
router port streamContainerRef { method: Post, path: [], body, headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/" <> show (nextGraphId streamContainer)) (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  liftEffect $ modify_ (addGraphToStreamContainer $ fromFoldable payload) streamContainerRef
  created
-- GET /all
router port streamContainerRef { method: Get, path : [ "all" ], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  let graph = getUnionGraph streamContainer
  payload <- write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ Array.fromFoldable graph
  ok' (header "Content-Type" "text/turtle") payload
-- GET /{id}
router port streamContainerRef { method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case Integer.fromString (path !@ 0) of 
  Nothing -> notFound
  Just i -> case getGraphFromStreamContainer i streamContainer of 
    Nothing -> notFound
    Just graph -> do
      payload <- write ("http://localhost:" <> show port <> "/" <> show i) (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ Array.fromFoldable graph
      ok' (header "Content-Type" "text/turtle") payload
-- POST /window
router port streamContainerRef { method: Post, path: [ "window" ], body, headers: (Headers headers) } = do
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  case addWindowToStreamContainer <$> quadsToWindow payload of 
    Nothing -> badRequest "Could not parse window"
    Just window -> do
      liftEffect $ modify_ window streamContainerRef
      created
router _ _ _ = notFound

main :: ServerM
main = do
  streamContainerRef <- new $ emptyStreamContainer
  args <- argv
  let port = fromMaybe 8080 do
        portString <- args !! 2
        Integer.fromString portString
  serve port (router port streamContainerRef) $ log $ "Server now up on port " <> show port
