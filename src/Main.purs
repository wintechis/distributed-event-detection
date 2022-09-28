module Main where

import Prelude

import Data.Array (concat, dropEnd, length, mapWithIndex, range, (!!), (:))
import Data.Formatter.Interval (formatInterval)
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
import HTTPure (Method(..), Request, ResponseM, ServerM, created, header, notFound, ok', serve, toString)
import HTTPure.Headers (Headers(..))
import N3 (Format(..), parse, write)
import Node.Process (argv)
import RDF (Graph, Quad, Term, blankNode, defaultGraph, literalType, namedNode, namedNode', quad)
import RDF.Prefixes (Prefix(..), ldp, rdf, xsd)

ldpsc :: Prefix
ldpsc = Prefix "https://solid.ti.rw.fau.de/public/ns/stream-containers#"

data StreamContainer = StreamContainer (Array Graph) (Array Window)

data Window = Window Term Term Term IsoDuration

emptyStreamContainer :: StreamContainer
emptyStreamContainer = StreamContainer [] []

addGraphToStreamContainer :: Graph -> StreamContainer -> StreamContainer
addGraphToStreamContainer graph (StreamContainer graphArray windowArray) = StreamContainer (graph : graphArray) windowArray

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

formatForMIME :: Maybe String -> Format
formatForMIME (Just "text/turtle") = Turtle
formatForMIME (Just "application/trig") = TriG
formatForMIME (Just "application/n-triples") = NTriples
formatForMIME (Just "application/n-quads") = NQuads
formatForMIME Nothing = Turtle
formatForMIME _ = Turtle

router :: Int -> Ref StreamContainer -> Request -> ResponseM
router port streamContainerRef { method: Get, path: [], headers: (Headers headers) } = do
  streamContainer <- liftEffect $ read streamContainerRef
  payload <- write ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Accept") headers) $ streamContainerToQuads streamContainer
  ok' (header "Content-Type" "text/turtle") payload
router port streamContainerRef { method: Post, path: [], body, headers: (Headers headers) } = do
  bodyString <- toString body
  payload <- parse ("http://localhost:" <> show port <> "/") (formatForMIME $ lookup (CaseInsensitiveString "Content-Type") headers) bodyString
  liftEffect $ modify_ (addGraphToStreamContainer $ fromFoldable payload) streamContainerRef
  created
router _ _ _ = notFound

main :: ServerM
main = do
  streamContainerRef <- new emptyStreamContainer
  args <- argv
  let port = fromMaybe 8080 do
        portString <- args !! 2
        fromString portString
  serve port (router port streamContainerRef) $ log $ "Server now up on port " <> show port
