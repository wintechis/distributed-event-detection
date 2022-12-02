module Main where

import Prelude

import Data.Array (concatMap, filter, mapWithIndex, (:))
import Data.Array as Array
import Data.Map (Map, fromFoldable)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

type Time = Int

data Interval = Interval Time Time
instance showInterval :: Show Interval where
  show (Interval start end) = "[" <> show start <> ", " <> show end <> "]"
derive instance eqInterval :: Eq Interval
derive instance ordInterval :: Ord Interval

data Term = Variable String | Constant String
instance showTerm :: Show Term where
  show (Variable name) = "?" <> name
  show (Constant name) = name

data Predicate = Predicate String
instance showPredicate :: Show Predicate where
  show (Predicate name) = name
derive instance eqPredicate :: Eq Predicate
derive instance ordPredicate :: Ord Predicate

data Formula = Formula Predicate (Array Term) | BoxPlus Interval Formula | BoxMinus Interval Formula | DiamondPlus Interval Formula | DiamondMinus Interval Formula
instance showFormula :: Show Formula where
  show (Formula predicate terms) = show predicate <> "(" <> joinWith ", " (show <$> terms) <> ")"
  show (BoxPlus interval formula) = "□+_" <> show interval <> " " <> show formula
  show (BoxMinus interval formula) = "□-_" <> show interval <> " " <> show formula
  show (DiamondPlus interval formula) = "◇+_" <> show interval <> " " <> show formula
  show (DiamondMinus interval formula) = "◇-_" <> show interval <> " " <> show formula

data Rule = Rule Formula (Array Formula)
instance showRule :: Show Rule where
  show (Rule head body) = show head <> " ← " <> joinWith " ∧ " (show <$> body)

type Program = Array Rule

lightjam :: Rule
lightjam = Rule (Formula (Predicate "light_jam") [ Variable "car" ]) [ BoxMinus (Interval 0 15) (Formula (Predicate "less_than") [ Variable "speed", Constant "30" ]) ]

mediumjam :: Rule
mediumjam = Rule (Formula (Predicate "medium_jam") [ Variable "car" ]) [ Formula (Predicate "light_jam") [ Variable "car" ], DiamondMinus (Interval 0 30) (BoxMinus (Interval 0 3) (Formula (Predicate "speed") [ Variable "car", Constant "0" ])) ]

heavyjam :: Rule
heavyjam = Rule (Formula (Predicate "heavy_jam") [ Variable "car" ]) [ Formula (Predicate "light_jam") [ Variable "car" ], BoxMinus (Interval 0 30) (DiamondMinus (Interval 0 10) (BoxMinus (Interval 0 3) (Formula (Predicate "speed") [ Variable "car", Constant "0" ]))) ]

jam :: Program
jam = [ lightjam, mediumjam, heavyjam ]

testFormula :: Formula
testFormula = BoxPlus (Interval 3 10) (BoxPlus (Interval 4 15) (Formula (Predicate "p") [ Variable "a", Constant "c" ]))

testRule :: Rule
testRule = Rule (Formula (Predicate "z") [ Variable "a" ]) [ testFormula ]

filterVars :: Array Term -> Array Term
filterVars = filter f
  where
    f t = case t of 
      (Variable _) -> true
      (Constant _) -> false

filterConsts :: Array Term -> Array Term
filterConsts = filter f
  where
    f t = case t of 
      (Variable _) -> false
      (Constant _) -> true

normalFormula :: Formula -> Tuple Program (Tuple Predicate (Array Term))
normalFormula (Formula predicate terms) = Tuple [] (Tuple predicate terms)
normalFormula formula@(BoxPlus (Interval start end) (Formula (Predicate predicate) terms)) = Tuple [ Rule (Formula newPred terms) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (BoxPlus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Formula (newPred pred) ts) [ BoxPlus (Interval start end) (Formula pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(BoxMinus (Interval start end) (Formula (Predicate predicate) terms)) = Tuple [ Rule (Formula newPred (filterVars terms)) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("boxMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (BoxMinus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Formula (newPred pred) ts) [ BoxMinus (Interval start end) (Formula pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("boxMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(DiamondPlus (Interval start end) (Formula (Predicate predicate) terms)) = Tuple [ Rule (Formula newPred (filterVars terms)) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("diamondPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (DiamondPlus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Formula (newPred pred) ts) [ DiamondPlus (Interval start end) (Formula pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("diamondPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(DiamondMinus (Interval start end) (Formula (Predicate predicate) terms)) = Tuple [ Rule (Formula newPred terms) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("diamondMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (DiamondMinus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Formula (newPred pred) ts) [ DiamondMinus (Interval start end) (Formula pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("diamondMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))

normalRule :: Rule -> Program
normalRule (Rule head body) = [ Rule head (map (\(Tuple _ (Tuple predicate terms)) -> Formula predicate terms) tuples) ] <> concatMap (\(Tuple program _) -> program) tuples
  where
    tuples :: Array (Tuple Program (Tuple Predicate (Array Term)))
    tuples = map normalFormula body

normalForm :: Program -> Program
normalForm = concatMap normalRule

formulaPredicate :: Formula -> Predicate
formulaPredicate (Formula predicate _) = predicate
formulaPredicate (BoxPlus _ formula) = formulaPredicate formula
formulaPredicate (BoxMinus _ formula) = formulaPredicate formula
formulaPredicate (DiamondPlus _ formula) = formulaPredicate formula
formulaPredicate (DiamondMinus _ formula) = formulaPredicate formula

rulePredicates :: Rule -> Set Predicate
rulePredicates (Rule head body) = Set.insert (formulaPredicate head) (Set.fromFoldable $ map formulaPredicate body)

programPredicates :: Program -> Set Predicate
programPredicates program = Set.unions $ map rulePredicates program

getIntervallsForPredicates :: Program -> Map Predicate (Set Interval)
getIntervallsForPredicates program = fromFoldable $ Set.map (\p -> Tuple p (getIntervallsForPredicate p)) (programPredicates program)
  where
    getIntervallsForPredicate :: Predicate -> Set Interval
    getIntervallsForPredicate predicate = Set.fromFoldable $ concatMap (\(Rule head body) -> getIntervallsForPredicateFormula head <> concatMap getIntervallsForPredicateFormula body) program
      where
        getIntervallsForPredicateFormula :: Formula -> Array Interval
        getIntervallsForPredicateFormula (Formula _ _) = []
        getIntervallsForPredicateFormula (BoxPlus interval (Formula predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula (BoxMinus interval (Formula predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula (DiamondPlus interval (Formula predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula (DiamondMinus interval (Formula predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula _ = []

showIntervalForPredicates :: Map Predicate (Set Interval) -> String
showIntervalForPredicates fiMap = joinWith "\n" $ map (\(Tuple predicate intervals) -> show predicate <> ":\t" <> (joinWith ", " $ map show $ Array.fromFoldable intervals)) $ Map.toUnfoldable fiMap

dockerCompose :: Program -> String
dockerCompose program = "services:\n" <> (joinWith "\n" $ mapWithIndex (\i (Tuple predicate intervals) -> "  " <> show predicate <> "-stream-container:\n" <> "    command: node index.js -p " <> show (9000 + i) <> joinWith "" (map (\(Interval start end) -> " -w \"#window-" <> show start <> "-" <> show end <> " http://ex.org/inWindow http://ex.org/timestamp " <> show start <> " " <> show end <> "\"") (Set.toUnfoldable intervals :: Array Interval)) <> "\n    ports:\n      - \"" <> show (9000 + i) <> ":" <> show (9000 + i) <> "\"\n    image: stream-container:latest") streamContainerList)
  where
    streamContainerList :: Array (Tuple Predicate (Set Interval))
    streamContainerList = Map.toUnfoldable $ getIntervallsForPredicates $ normalForm program

main :: Effect Unit
main = do
  logShow jam
  logShow $ normalForm jam
  logShow $ programPredicates $ normalForm jam
  log $ showIntervalForPredicates $ getIntervallsForPredicates $ normalForm jam
  log $ dockerCompose $ normalForm jam
  writeTextFile UTF8 "docker-compose.yml" (dockerCompose $ normalForm jam)
