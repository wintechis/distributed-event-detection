module Main where

import Prelude

import Data.Array (concatMap, filter, (:))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)

type Time = Int

data Interval = Interval Time Time
instance showInterval :: Show Interval where
  show (Interval start end) = "[" <> show start <> ", " <> show end <> "]"

data Term = Variable String | Constant String
instance showTerm :: Show Term where
  show (Variable name) = "?" <> name
  show (Constant name) = name

data Predicate = Predicate String
instance showPredicate :: Show Predicate where
  show (Predicate name) = name

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

main :: Effect Unit
main = do
  logShow jam
  logShow $ normalForm jam
