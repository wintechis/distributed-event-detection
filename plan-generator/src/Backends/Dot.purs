module Backends.Dot where

import Prelude

import Data.Array (fromFoldable)
import Data.DotLang (Definition(..), Graph(..), Node(..))
import Data.DotLang.Attr.Node (Attr(..), LabelValue(..), RecordLabelValue(..), ShapeType(..))
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (toText)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.String (joinWith)
import DatalogMTL (Formula(..), Interval(..), Predicate(..), Program, Rule(..), Term(..), normalForm)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import MainNew (Plan(..), ReasoningNode(..), StreamNode(..), Window(..), createPlan)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

planToGraph :: Plan -> Graph
planToGraph (Plan rNodes sNodes wToS) = DiGraph $ (map planToGraphReasoningNode rNodes <> map (planToGraphStreamNode wToS) sNodes)

planToGraphReasoningNode :: ReasoningNode -> Definition
planToGraphReasoningNode node@(ReasoningNode reasoningType _ _) = NodeDef $ Node ("\"" <> show node <> "\"") [ Shape Circle, Node.Label $ Node.TextLabel $ show reasoningType ]

planToGraphStreamNode :: Map StreamNode (Set Window) -> StreamNode -> Definition
planToGraphStreamNode wToS sNode@(StreamNode pred) = NodeDef $ Node ("\"" <> show pred <> "\"") [ Shape Node.Record, Node.Label $ RecordLabel $ SubRecord $ [ { fieldId: Nothing, value: Base $ show pred }, { fieldId: Nothing, value: SubRecord (map (\(Window _ start end) -> { fieldId: Nothing, value: Node.Base $ "[" <> show start <> ", " <> show end <> "]"}) windows) } ] ]
  where
    windows :: Array Window
    windows = fromMaybe [] $ fromFoldable <$> Map.lookup sNode wToS

speedLessThanEqual30 :: Rule
speedLessThanEqual30 = Rule (Pred (Predicate "speed_less_than_equal_30") [ Variable "car" ]) [ Pred (Predicate "speed") [ Variable "car", Variable "speed" ], Pred (Predicate "less_than_equal") [ Variable "speed", Constant "30" ] ]

speed0 :: Rule
speed0 = Rule (Pred (Predicate "speed_less_than_equal_0") [ Variable "car" ]) [ Pred (Predicate "speed") [ Variable "car", Variable "speed" ], Pred (Predicate "less_than_equal") [ Variable "speed", Constant "0" ] ]

lightjam :: Rule
lightjam = Rule (Pred (Predicate "light_jam") [ Variable "car" ]) [ BoxMinus (Interval 0 15) (Pred (Predicate "speed_less_than_equal_30") [ Variable "car" ]) ]

mediumjam :: Rule
mediumjam = Rule (Pred (Predicate "medium_jam") [ Variable "car" ]) [ Pred (Predicate "light_jam") [ Variable "car" ], DiamondMinus (Interval 0 30) (BoxMinus (Interval 0 3) (Pred (Predicate "speed_less_than_equal_0") [ Variable "car" ])) ]

heavyjam :: Rule
heavyjam = Rule (Pred (Predicate "heavy_jam") [ Variable "car" ]) [ Pred (Predicate "light_jam") [ Variable "car" ], BoxMinus (Interval 0 30) (DiamondMinus (Interval 0 10) (BoxMinus (Interval 0 3) (Pred (Predicate "speed_less_than_equal_0") [ Variable "car" ]))) ]

jam :: Program
jam = [ speedLessThanEqual30, speed0, lightjam, mediumjam, heavyjam ]

showProgram :: Program -> String
showProgram rules = "[\n" <> joinWith "\n" (map (\r -> "  " <> show r) rules) <> "\n]"

main :: Effect Unit
main = do
  logShow $ createPlan $ normalForm jam
  log $ toText $ planToGraph $ createPlan $ normalForm jam
  writeTextFile UTF8 "plan.dot" (toText $ planToGraph $ createPlan $ normalForm jam)