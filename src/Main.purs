module Main where

import Prelude

import Data.Array (concat, dropEnd, filter, head, length, mapWithIndex, range, snoc, (!!))
import Data.Array as Array
import Data.Either (hush)
import Data.Formatter.Interval (formatInterval)
import Data.Formatter.Parser.Interval (parseIsoDuration)
import Data.Int (fromString)
import Data.Interval (Interval(..))
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (fromFoldable)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, modify_, new, read)
import HTTPure (Method(..), Request, ResponseM, ServerM, badRequest, created, header, notFound, ok', serve, toString, (!@))
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Node.Process (argv)
import Parsing (runParser)
import RDF (Graph, Quad, Term, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, value)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)

ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data StreamContainer = StreamContainer (Array Graph) (Array Window)

data Window = Window Term Term Term IsoDuration

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

streamContainerToQuads :: StreamContainer -> Array Quad
streamContainerToQuads (StreamContainer graphArray windowArray) = [
  quad (namedNode "") (namedNode' rdf "type") (namedNode' ldpsc "StreamContainer") defaultGraph
] <>
  map (\i -> quad (namedNode "") (namedNode' ldp "contains") (namedNode $ "/" <> show i) defaultGraph) (dropEnd 1 (range 0 (length graphArray))) <>
  (concat $ mapWithIndex windowToQuads windowArray)

windowToQuads :: Int -> Window -> Array Quad
windowToQuads i (Window memberRelation membershipResource contentTimestampRelation duration) = [
  quad (namedNode "") (namedNode' ldpsc "window") window defaultGraph,
  quad window (namedNode' ldp "hasMemberRelation") memberRelation defaultGraph,
  quad window (namedNode' ldp "hasMembershipResource") membershipResource defaultGraph,
  quad window (namedNode' ldpsc "hasContentTimestampRelation") contentTimestampRelation defaultGraph,
  quad window (namedNode' ldpsc "logical") (literalType (formatInterval (DurationOnly duration)) (namedNode' xsd "string")) defaultGraph
]
  where
    window = blankNode $ "window-" <> show i

quadsToWindow :: Array Quad -> Maybe Window
quadsToWindow quads = do
  memberRelation <- head $ filter (\quad -> predicate quad == namedNode' ldp "hasMemberRelation") quads
  membershipResource <- head $ filter (\quad -> predicate quad == namedNode' ldp "hasMembershipResource") quads
  contentTimestampRelation <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "hasContentTimestampRelation") quads
  logical <- head $ filter (\quad -> predicate quad == namedNode' ldpsc "logical") quads :: Maybe Quad
  duration <- hush $ runParser (value $ object logical) parseIsoDuration
  pure $ Window (object memberRelation) (object membershipResource) (object contentTimestampRelation) duration

formatForMIME :: Maybe String -> Format
formatForMIME (Just "text/turtle") = Turtle
formatForMIME (Just "application/trig") = TriG
formatForMIME (Just "application/n-triples") = NTriples
formatForMIME (Just "application/n-quads") = NQuads
formatForMIME Nothing = Turtle
formatForMIME _ = Turtle

router :: Int -> Ref StreamContainer -> Request -> ResponseM
-- GET /
router port streamContainerRef { method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  payload <- write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ streamContainerToQuads streamContainer
  ok' (header "Content-Type" "text/turtle") payload
-- POST /
router port streamContainerRef { method: Post, path: [], body, headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/" <> show (nextGraphId streamContainer)) (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  liftEffect $ modify_ (addGraphToStreamContainer $ fromFoldable payload) streamContainerRef
  created
-- GET /{id}
router port streamContainerRef { method: Get, path, headers: (Headers headers) } | length path == 1 = (liftEffect $ read streamContainerRef) >>= \streamContainer -> case fromString (path !@ 0) of 
  Nothing -> notFound
  Just i -> case getGraphFromStreamContainer i streamContainer of 
    Nothing -> notFound
    Just graph -> do
      payload <- write ("http://localhost:" <> show port <> "/" <> show i) (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ Array.fromFoldable graph
      ok' (header "Content-Type" "text/turtle") payload
-- POST /window
router port streamContainerRef { method: Post, path: ["window"], body, headers: (Headers headers) } = do
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
        fromString portString
  serve port (router port streamContainerRef) $ log $ "Server now up on port " <> show port
