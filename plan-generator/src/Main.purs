module Main where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Map (toUnfoldable)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DatalogMTL (Formula(..), Interval(..), Predicate(..), Program, Rule(..), Term(..), getIntervallsForPredicates, normalForm)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

data Window = Window Int Int StreamContainer

data StreamContainer = StreamContainer Term

data Agent = And Window (Array Window) | Box Window Window | Diamond Window Window

data LogicalPlan = LogicalPlan (Array Agent)

--getLogicalPlan :: Program -> LogicalPlan
--getLogicalPlan program = LogicalPlan []
--  where
--    streamContainers :: Map String StreamContainer
--    streamContainers = foldl (\m r -> ) Map.empty program
--      where
--        

dockerCompose :: Program -> String
dockerCompose program = "services:\n"
  <> (joinWith "\n" $ mapWithIndex (\i (Tuple predicate intervals) -> "  "
    <> show predicate
    <> "-stream-container:\n"
    <> "    command: node index.js -p "
    <> show (9000 + i)
    <> joinWith "" (map (\(Interval start end) -> " -w \"#window-" <> show start <> "-" <> show end <> " http://ex.org/inWindow http://ex.org/timestamp http://ex.org/poisened http://ex.org/isPoisonous "  <> show start <> " " <> show end <> "\"") (Set.toUnfoldable intervals :: Array Interval))
    <> "\n    ports:\n      - \""
    <> show (9000 + i)
    <> ":"
    <> show (9000 + i)
    <> "\"\n    image: stream-container:latest") streamContainerList)
    where
      streamContainerList :: Array (Tuple Predicate (Set Interval))
      streamContainerList = Map.toUnfoldable $ getIntervallsForPredicates $ normalForm program

dot :: Program -> String
dot program = "digraph G {\n" <> joinWith "\n" (map scNode (toUnfoldable $ getIntervallsForPredicates program)) <> "\n" <> joinWith "\n" (map agentNode program) <> "\n}"
  where
  scNode :: Tuple Predicate (Set Interval) -> String
  scNode (Tuple (Predicate predicate) intervals) = "  " <> predicate <> " [shape=record, label=\"{" <> predicate <> (if Set.size intervals > 0 then ("|{" <> joinWith "|" (map (\(Interval start end) -> "<w_" <> show start <> "_" <> show end <> ">" <> "[" <> show start <> "," <> show end <> "]") (Array.fromFoldable intervals)) <> "}") else "") <> "}\"];"
  agentNode :: Rule -> String
  agentNode (Rule (Pred (Predicate headPred) _) [ BoxPlus (Interval start end) (Pred (Predicate (bodyPred)) _) ]) = "  " <> agentName <> " [shape=circle, label=\"□\"];\n  " <> bodyPred <> ":<w_" <> show start <> "_" <> show end <> ">" <> " -> " <> agentName <> ";\n  " <> agentName <> " -> " <> headPred <> ";"
    where
      agentName = "agent_" <> bodyPred <> "_BoxPlus_" <> headPred
  agentNode (Rule (Pred (Predicate headPred) _) [ BoxMinus (Interval start end) (Pred (Predicate (bodyPred)) _) ]) = "  " <> agentName <> " [shape=circle, label=\"□\"];\n  " <> bodyPred <> ":<w_" <> show (negate start) <> "_" <> show (negate end) <> ">" <> " -> " <> agentName <> ";\n  " <> agentName <> " -> " <> headPred <> ";"
    where
      agentName = "agent_" <> bodyPred <> "_BoxMinus_" <> headPred
  agentNode (Rule (Pred (Predicate headPred) _) [ DiamondPlus (Interval start end) (Pred (Predicate (bodyPred)) _) ]) = "  " <> agentName <> " [shape=circle, label=\"◇\"];\n  " <> bodyPred <> ":<w_" <> show start <> "_" <> show end <> ">" <> " -> " <> agentName <> ";\n  " <> agentName <> " -> " <> headPred <> ";"
    where
      agentName = "agent_" <> bodyPred <> "_DiamondPlus_" <> headPred
  agentNode (Rule (Pred (Predicate headPred) _) [ DiamondMinus (Interval start end) (Pred (Predicate (bodyPred)) _) ]) = "  " <> agentName <> " [shape=circle, label=\"◇\"];\n  " <> bodyPred <> ":<w_" <> show (negate start) <> "_" <> show (negate end) <> ">" <> " -> " <> agentName <> ";\n  " <> agentName <> " -> " <> headPred <> ";"
    where
      agentName = "agent_" <> bodyPred <> "_DiamondMinus_" <> headPred
  agentNode (Rule (Pred (Predicate headPred) _) body) = "  " <> agentName <> " [shape=circle, label=\"∧\"];\n" <> joinWith "\n" (map (\f -> "  " <> getPredicateString f <> ":<w_0_0> -> " <> agentName <> ";") body) <> "  " <> agentName <> " -> " <> headPred <> ";"
    where
      getPredicateString :: Formula -> String
      getPredicateString (Pred (Predicate predicate) _) = predicate
      getPredicateString _ = "// Something went wrong here (Formula too deep)!"
      agentName = "agent_" <> joinWith "_" (map getPredicateString body) <> "_BoxPlus_" <> headPred
  agentNode (Rule _ _ ) = "// Something went wrong here (No Match)!"

showProgram :: Program -> String
showProgram rules = "[\n" <> joinWith "\n" (map (\r -> "  " <> show r) rules) <> "\n]"

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

main :: Effect Unit
main = do
  --log $ showProgram jam
  log $ showProgram $ normalForm jam
  writeTextFile UTF8 "plan.dot" (dot $ normalForm jam)
  writeTextFile UTF8 "docker-compose.yml" (dockerCompose $ normalForm jam)
