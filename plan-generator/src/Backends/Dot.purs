module Backends.Dot where

import Prelude

import Data.Array (concat, fromFoldable, mapWithIndex, (:))
import Data.DotLang (Definition(..), Edge(..), EdgeType(..), Graph(..), Node(..))
import Data.DotLang.Attr.Node (Attr(..), LabelValue(..), RecordLabelValue(..), ShapeType(..))
import Data.DotLang.Attr.Node as Node
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import DatalogMTL (Aggregation(..), Formula(..), Interval(..), Predicate(..), Program, Rule(..), Term(..))
import Frontend (Plan(..), ReasoningNode(..), StreamNode(..), Window(..))

planToGraph :: Plan -> Graph
planToGraph (Plan rNodes sNodes wToS) = DiGraph $ (mapWithIndex planToGraphReasoningNode rNodes <> map (planToGraphStreamNode wToS) sNodes) <> (concat $ mapWithIndex planToGraphEdges rNodes)

planToGraphReasoningNode :: Int -> ReasoningNode -> Definition
planToGraphReasoningNode i (ReasoningNode reasoningType _ _) = NodeDef $ Node ("rn" <> show i) [ Shape Circle, Node.Label $ Node.TextLabel $ show reasoningType ]

planToGraphStreamNode :: Map StreamNode (Set Window) -> StreamNode -> Definition
planToGraphStreamNode wToS sNode@(StreamNode pred _) = NodeDef $ Node (show pred) [ Shape Node.Record, Node.Label $ RecordLabel $ SubRecord $ [
      { fieldId: Nothing, value: SubRecord [
        { fieldId: Nothing, value: Base (show pred)},
        { fieldId: Nothing, value: SubRecord (
          map (\(Window _ start end) -> { fieldId: Just $ show start <> "_" <> show end, value: Node.Base $ "[" <> show start <> ", " <> show end <> "]"}) windows
        ) }
      ] }
    ]
  ]
  where
    windows :: Array Window
    windows = fromMaybe [] $ fromFoldable <$> Map.lookup sNode wToS

planToGraphEdges :: Int -> ReasoningNode -> Array Definition
planToGraphEdges i (ReasoningNode _ windows (StreamNode pred _)) = (EdgeDef $ Edge Forward ("rn" <> show i) (show pred) []) : (map (\(Window (StreamNode p _) start end) -> EdgeDef $ Edge Forward (show p <> ":<" <> show start <> "_" <> show end <> ">") ("rn" <> show i) []) windows)

-- New use case
stopingForRedLight :: Rule
stopingForRedLight = Rule (Pred (Predicate "stoping_for_red_light") [Variable "car"]) [ DiamondMinus (Interval 0 5) (Pred (Predicate "speed") [ Variable "car", Constant "0" ]), Pred (Predicate "traffic_light") [ Constant "red" ] ]

avgSpeed5 :: Rule
avgSpeed5 = AggrRule (Pred (Predicate "avg_speed_5") [ Variable "car", Variable "avg_speed" ]) Average (Variable "avg_speed") (DiamondMinus (Interval 0 5) (Pred (Predicate "speed") [ Variable "car", Variable "speed" ]))

trafficJam :: Rule 
trafficJam = Rule (Pred (Predicate "traffic_jam") [Variable "car"]) [ BoxMinus (Interval 0 60) (DiamondMinus (Interval 0 5) (Pred (Predicate "speed") [ Variable "car", Constant "0" ])) ]

runningExample :: Program
runningExample = [
  stopingForRedLight,
  avgSpeed5,
  trafficJam
]