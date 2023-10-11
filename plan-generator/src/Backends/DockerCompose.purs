module Backends.DockerCompose where

import Prelude

import Backends.Dot (runningExample)
import Data.Array (concat, filter, fold, fromFoldable, mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DatalogMTL (Predicate(..), Program, normalForm)
import Dodo (Doc, break, foldWithSeparator, indent, plainText, print, text, twoSpaces)
import Effect (Effect)
import Effect.Console (log, logShow)
import Frontend (Plan(..), ReasoningNode(..), ReasoningType(..), StreamNode(..), Window(..), createPlan)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import RDF (Term, literalType, namedNode, namedNode', value)
import RDF.Prefixes (xsd)

data Parameters = Parameters {
  inputs :: Array Input
}

data Input = Input Term Predicate (Array Term)

data Compose = Compose (Array Service) (Array Network) (Array Volume) (Array Config) (Array Secret)

data Service = Service String { image :: Image, command :: Array String, ports :: Map Int Int}

data Image = StreamContainer | ReasoningAgent
instance showImage :: Show Image where
  show StreamContainer = "stream-container:latest"
  show ReasoningAgent = "reasoning-agent:latest"

data Network = Network

data Volume = Volume

data Config = Config 

data Secret = Secret

planToCompose :: Parameters -> Plan -> Compose
planToCompose parameters (Plan rNodes sNodes wToS) = Compose (mapWithIndex (streamNodeToService parameters wToS) sNodes <> mapWithIndex reasoningNodeToService rNodes) [] [] [] []

streamNodeToService :: Parameters -> Map StreamNode (Set Window) -> Int -> StreamNode -> Service
streamNodeToService (Parameters parameters) wToS i sNode@(StreamNode pred _) = Service (show pred) { image: StreamContainer, ports: Map.singleton (9000 + i) 8080, command: [
  "node",
  "index.js",
  "-p",
  "http://vocab.ex.org/" <> show pred
] <> (concat (map (\(Window _ start end) -> [
  "-w",
  "#window" <> show start <> "_" <> show end <> " " <> show start <> " " <> show end 
]) $ fromFoldable $ fromMaybe Set.empty $ Map.lookup sNode wToS)) <> (concat (map (\(Input subject _ objects) -> [
  "-d",
  value subject <> " [" <> joinWith "," (map value objects) <> "]"
]) $ filter (\(Input _ predicate _) -> predicate == pred) parameters.inputs))
}

reasoningNodeToService :: Int -> ReasoningNode -> Service
reasoningNodeToService i (ReasoningNode reasoningType windows (StreamNode pred terms)) = Service ("rn" <> show i) { image: ReasoningAgent, ports: Map.empty, command: [
  "node",
  "index.js",
  rType reasoningType,
  "-g",
  "http://" <> show pred <> ":8080 " <> show pred <> " " <> joinWith " " (map show $ terms)
] <> concat(map (\(Window (StreamNode wPred wTerms) start end) -> ["-s", "http://" <> show wPred <> ":8080/#window" <> show start <> "_" <> show end <> " " <> show wPred <> " " <> joinWith " " (map show $ wTerms) ]) windows) }
  where
    rType And = "AND"
    rType Box = "BOX"
    rType Diamond = "DIAMOND"
    rType Agg = "AGG"

composeToDoc :: Compose -> Doc Void
composeToDoc (Compose services _ _ _ _) = text "services:" <> break <> (indent $ foldWithSeparator break $ map serviceToDoc services)

serviceToDoc :: Service -> Doc Void
serviceToDoc (Service name config) = text name <> text ":" <> break <> (indent $ foldWithSeparator break [
  text "image: " <> text (show config.image),
  text "command: [" <> foldWithSeparator (text ", ") (map (\c -> text "\"" <> text c <> text "\"") config.command) <> text "]",
  if Map.size config.ports > 0 then text "ports:" <> break <> (indent $ fold $ map (\(Tuple p1 p2) -> text "- " <> text (show p1) <> text ":" <> text (show p2)) $ Map.toUnfoldable config.ports) else text ""
])

showProgram :: Program -> String
showProgram rules = "[\n" <> joinWith "\n" (map (\r -> "  " <> show r) rules) <> "\n]"

writeCompose :: Program -> Effect Unit
writeCompose program = do
  let parameters = Parameters { inputs: [] }
  writeTextFile UTF8 "compose.yaml" (print plainText twoSpaces $ composeToDoc $ planToCompose parameters $ createPlan $ normalForm program)

main :: Effect Unit
main = do
  log $ showProgram runningExample
  log ""
  log $ showProgram $ normalForm runningExample
  log ""
  logShow $ createPlan $ normalForm runningExample
  log ""
  let parameters = Parameters { inputs: [
    Input (namedNode "http://cars.ex.org/car1") (Predicate "speed") [
      literalType "10" (namedNode' xsd "integer"),
      literalType "12" (namedNode' xsd "integer"),
      literalType "14" (namedNode' xsd "integer"),
      literalType "16" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "20" (namedNode' xsd "integer"),
      literalType "20" (namedNode' xsd "integer"),
      literalType "20" (namedNode' xsd "integer"),
      literalType "15" (namedNode' xsd "integer")
    ],
    Input (namedNode "http://cars.ex.org/car2") (Predicate "speed") [
      literalType "15" (namedNode' xsd "integer"),
      literalType "15" (namedNode' xsd "integer"),
      literalType "15" (namedNode' xsd "integer"),
      literalType "15" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer"),
      literalType "18" (namedNode' xsd "integer")
    ]
  ] }
  log $ print plainText twoSpaces $ composeToDoc $ planToCompose parameters $ createPlan $ normalForm runningExample
  writeTextFile UTF8 "compose.yaml" (print plainText twoSpaces $ composeToDoc $ planToCompose parameters $ createPlan $ normalForm runningExample)