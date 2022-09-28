module Main where

import Prelude

import Data.Array (concat, dropEnd, length, mapWithIndex, range, (!!), (:))
import Data.Either (Either, hush)
import Data.Formatter.Interval (formatInterval)
import Data.Int (fromString)
import Data.Interval (Interval(..), second)
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Effect.Console (log)
import HTTPure (Request, ResponseM, ServerM, header, ok', serve)
import N3 (Format(..), write)
import Node.Process (argv)
import Partial.Unsafe (unsafePartial)
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

router :: Int -> Request -> ResponseM
router port _ = do
  payload <- write ("http://localhost:" <> show port <> "/") Turtle $ streamContainerToQuads emptyStreamContainer
  ok' (header "Content-Type" "text/turtle") payload

main :: ServerM
main = do
  args <- argv
  let port = fromMaybe 8080 do
        portString <- args !! 2
        fromString portString
  serve port (router port) $ log $ "Server now up on port " <> show port
