module Main where

import Prelude

import Data.Array (length, (:))
import Data.Array.Partial as Array
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import Effect.Console (log, logShow)

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
derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

data Predicate = Predicate String
instance showPredicate :: Show Predicate where
  show (Predicate name) = name
derive instance eqPredicate :: Eq Predicate
derive instance ordPredicate :: Ord Predicate

data Formula = Formula Predicate (Array Term) | BoxPlus Interval Formula -- | BoxMinus Interval Formula | DiamondPlus Interval Formula | DiamondMinus Interval Formula
instance showFormula :: Show Formula where
  show (Formula predicate terms) = show predicate <> "(" <> joinWith ", " (show <$> terms) <> ")"
  show (BoxPlus interval formula) = "□+_" <> show interval <> " " <> show formula
--  show (BoxMinus interval formula) = "□-_" <> show interval <> " " <> show formula
--  show (DiamondPlus interval formula) = "◇+_" <> show interval <> " " <> show formula
--  show (DiamondMinus interval formula) = "◇-_" <> show interval <> " " <> show formula
derive instance eqFormula :: Eq Formula
derive instance ordFormula :: Ord Formula

formulaDepth :: Formula -> Int
formulaDepth (Formula _ _) = 0
formulaDepth (BoxPlus _ formula) = formulaDepth formula + 1
--formulaDepth (BoxMinus _ formula) = formulaDepth formula + 1
--formulaDepth (DiamondPlus _ formula) = formulaDepth formula + 1
--formulaDepth (DiamondMinus _ formula) = formulaDepth formula + 1

data Rule = Rule Formula (Array Formula)
instance showRule :: Show Rule where
  show (Rule head body) = show head <> " ← " <> joinWith " ∧ " (show <$> body)

type Program = Array Rule

--lightjam :: Rule
--lightjam = Rule (Formula (Predicate "light_jam") [ Variable "car" ]) [ BoxMinus (Interval 0 15) (Formula (Predicate "less_than") [ Variable "speed", Constant "30" ]) ]
--
--mediumjam :: Rule
--mediumjam = Rule (Formula (Predicate "medium_jam") [ Variable "car" ]) [ Formula (Predicate "light_jam") [ Variable "car" ], DiamondMinus (Interval 0 30) (BoxMinus (Interval 0 3) (Formula (Predicate "speed") [ Variable "car", Constant "0" ])) ]
--
--heavyjam :: Rule
--heavyjam = Rule (Formula (Predicate "heavy_jam") [ Variable "car" ]) [ Formula (Predicate "light_jam") [ Variable "car" ], BoxMinus (Interval 0 30) (DiamondMinus (Interval 0 10) (BoxMinus (Interval 0 3) (Formula (Predicate "speed") [ Variable "car", Constant "0" ]))) ]
--
--jam :: Program
--jam = [ lightjam, mediumjam, heavyjam ]

test :: Formula
test = BoxPlus (Interval 3 10) (BoxPlus (Interval 4 15) (Formula (Predicate "p") [ Variable "a", Constant "c" ]))

f :: Formula -> Tuple Program (Tuple Predicate (Array Term))
f (Formula predicate terms) = Tuple [] (Tuple predicate terms)
f formula@(BoxPlus (Interval start end) (Formula (Predicate predicate) terms)) = Tuple [ Rule (Formula newPred terms) [ formula ] ] (Tuple newPred terms)
  where
    newPred = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate)
f (BoxPlus (Interval start end) formula') = case f formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Formula (newPred pred) ts) [ BoxPlus (Interval start end) (Formula pred ts) ] : prog) (Tuple (newPred pred) ts)
    where
      newPred (Predicate predicate) = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate)
--f mapff formula@(BoxPlus (Interval start end) formula') = (f empty formula') (\(Tuple f mf) -> Tuple (insert _ _ mf) f)
--  where
--    newFormula = Formula (Predicate (predicate <> "_BoxPlus_" <> show start <> "_" <> show end)) terms

--ruleBody :: Rule -> Array Formula
--ruleBody (Rule _ body) = body
--
--ruleHead :: Rule -> Formula
--ruleHead (Rule head _) = head
--
--normalForm :: Program -> Program
--normalForm program = program
--
--isFormula :: Formula -> Boolean
--isFormula (Formula _ _) = true
--isFormula _ = false
--
--normalForm' :: Partial => Rule -> Program
--normalForm' (Rule head body) = map replace body
--  where
--    replace :: Formula -> Tuple Formula Program
--    replace formula = case formula of
--      f@(Formula _ _) -> Tuple f []
--      f -> Tuple (unfoldFormula f (Formula (Predicate "")) )
--normalForm' (Rule head body) | length body == 1 = if formulaDepth (Array.head body) > 0 then
--  [
--  ]
--  else 
--  []
--normalForm' _ = []
--
--unfoldFormula :: Formula -> Array Rule -> Formula -> Program
--unfoldFormula head accu (Formula predicate terms) = trace accu \_ -> Rule head [ Formula predicate terms ] : accu
--unfoldFormula head accu f@(BoxPlus _ (Formula (Predicate _) _)) = Rule head [ f ] : accu
--unfoldFormula head accu f@(BoxMinus _ (Formula (Predicate _) _)) = Rule head [ f ] : accu
--unfoldFormula head accu f@(DiamondPlus _ (Formula (Predicate _) _)) = Rule head [ f ] : accu
--unfoldFormula head accu f@(DiamondMinus _ (Formula (Predicate _) _)) = Rule head [ f ] : accu
--unfoldFormula head@(Formula (Predicate predicate) terms) accu (BoxPlus interval@(Interval start end) formula) = unfoldFormula newFormula (accu <> [ Rule head [ BoxPlus interval newFormula ] ]) formula
--  where
--    newFormula = Formula (Predicate $ predicate <> "_BoxPlus_" <> show start <> "_" <> show end) terms
--unfoldFormula head@(Formula (Predicate predicate) terms) accu (BoxMinus interval@(Interval start end) formula) = unfoldFormula newFormula (accu <> [ Rule head [ BoxMinus interval newFormula ] ]) formula
--  where
--    newFormula = Formula (Predicate $ predicate <> "_BoxMinus_" <> show start <> "_" <> show end) terms
--unfoldFormula head@(Formula (Predicate predicate) terms) accu (DiamondPlus interval@(Interval start end) formula) = unfoldFormula newFormula (accu <> [ Rule head [ DiamondPlus interval newFormula ] ]) formula
--  where
--    newFormula = Formula (Predicate $ predicate <> "_DiamondPlus_" <> show start <> "_" <> show end) terms
--unfoldFormula head@(Formula (Predicate predicate) terms) accu (DiamondMinus interval@(Interval start end) formula) = unfoldFormula newFormula (accu <> [ Rule head [ DiamondMinus interval newFormula ] ]) formula
--  where
--    newFormula = Formula (Predicate $ predicate <> "_DiamondMinus_" <> show start <> "_" <> show end) terms
--
--nextFormula :: Formula -> Maybe Formula
--nextFormula (Formula _ _) = Nothing
--nextFormula (BoxPlus _ formula) = Just formula
--nextFormula (BoxMinus _ formula) = Just formula
--nextFormula (DiamondPlus _ formula) = Just formula
--nextFormula (DiamondMinus _ formula) = Just formula

main :: Effect Unit
main = do
  logShow test
  logShow $ f test
  --logShow $ unfoldFormula (Formula (Predicate "medium_jam") [ Variable "car" ]) [] $ DiamondMinus (Interval 0 30) (BoxMinus (Interval 0 3) (Formula (Predicate "speed") [ Variable "car", Constant "0" ]))
