module MainOld where

import Prelude

{-import Data.Leibniz (type (~), coerce)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (logShow)

type Time = Number

data Term = Variable String | Constant String
instance showTerm :: Show Term where
  show (Variable name) = name
  show (Constant name) = name

data Interval = Open Time Time | Closed Time Time | LeftOpen Time Time | RightOpen Time Time
instance showInterval :: Show Interval where
  show (Open start end) = "(" <> show start <> "," <> show end <> ")"
  show (Closed start end) = "[" <> show start <> "," <> show end <> "]"
  show (LeftOpen start end) = "(" <> show start <> "," <> show end <> "]"
  show (RightOpen start end) = "[" <> show start <> "," <> show end <> ")"

data RuleHead
foreign import data RuleHeadTrue :: RuleHead
foreign import data RuleHeadFalse :: RuleHead

--data RuleHeadList

--class RuleHeadAnd (ruleheads :: Array RuleHead)

data Formula (rulehead :: RuleHead) rh
  = Predicate String (Array Term) (rulehead ~ RuleHeadTrue)
  | BoxPlus Interval (Formula rulehead rh)
  | BoxMinus Interval (Formula rulehead rh)
  | DiamondPlus Interval (Formula rulehead rh) (rulehead ~ RuleHeadFalse)
  | DiamondMinus Interval (Formula rulehead rh) (rulehead ~ RuleHeadFalse)
  | And (Array (Formula rulehead)) (rulehead ~ RuleHeadFalse)

instance showFormula :: Show (Formula rulehead) where
  show (Predicate pred terms _) = pred <> "(" <> joinWith "," (map show terms) <> ")"
  show (BoxPlus interval formula) = "BOXPLUS" <> show interval <> " " <> show formula
  show (BoxMinus interval formula) = "BOXMINUS" <> show interval <> " " <> show formula
  show (DiamondPlus interval formula _) = "DIAMONDPLUS" <> show interval <> " " <> show formula
  show (DiamondMinus interval formula _) = "DIAMONDMINUS" <> show interval <> " " <> show formula
  show (And formulas _) = "(" <> joinWith " ∧ " (map show formulas) <> ")"

predicate :: String -> Array Term -> Formula RuleHeadTrue
predicate pred terms = Predicate pred terms identity

boxPlus :: forall rulehead. Interval -> Formula rulehead -> Formula rulehead
boxPlus interval formula = BoxPlus interval formula

boxMinus :: forall rulehead. Interval -> Formula rulehead -> Formula rulehead
boxMinus interval formula = BoxMinus interval formula

diamondPlus :: Interval -> (forall (rh :: RuleHead). Formula rh) -> Formula RuleHeadFalse
diamondPlus interval formula = DiamondPlus interval formula identity

diamondMinus :: forall rh. Interval -> Formula rh -> Formula RuleHeadFalse
diamondMinus interval formula = DiamondMinus interval formula identity

data Rule = Fact (Formula RuleHeadFalse) Time | Rule (Formula RuleHeadTrue) (Formula RuleHeadFalse)
instance showRule :: Show Rule where
  show (Fact formula time) = show formula <> "@" <> show time
  show (Rule head body) = show head <> " ⟵ " <> show body

newtype Program = Program (Array Rule)
instance showProgramm :: Show Program where
  show (Program rules) = joinWith "\n" $ map show rules

--normalForm :: Program -> Program
--normalForm (Program rules) = Program $ map normalForm' rules
--  where
--    normalForm' fact@(Fact _ _) = fact
--    normalForm' (Rule head body) = 

--f1 :: Formula RuleHeadFalse
--f1 = diamondPlus (Open 5.0 7.0) (boxPlus (Closed 3.0 4.0) (predicate "p" [ Constant "a", Variable "X" ]))
--f1 :: Formula RuleHeadFalse
f1 = diamondMinus (Closed 4.0 5.0) $ predicate "p" [ Constant "a", Variable "X" ]
  
--p :: Program
--p = Program [
--    Fact (Predicate "p" [ (Constant "a"), (Constant "b") ]) 3.123,
--    Rule (DiamondMinus (Open 5.0 7.0) (BoxPlus (Closed 3.0 4.0) (Predicate "p" [ Constant "a", Variable "X" ]))) ((DiamondPlus (Open 4.0 5.0) ) (And [ Predicate "q" [ Variable "X", Constant "b" ], BoxPlus (Closed 4.0 5.0) (Predicate "r" [ Constant "b", Constant "c" ])]))
--  ]

main :: Effect Unit
main = do
  logShow ""--p
-}