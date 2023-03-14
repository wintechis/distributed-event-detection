module DatalogMTL.Semantics where

import Prelude

import Data.Array (fromFoldable, length, (:))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import DatalogMTL (Formula(..), Interval(..), Predicate(..), Program, Rule(..), Term(..), filterConsts)
import Effect (Effect)
import Effect.Console (logShow)

herbrandUniverse :: Program -> Set Term
herbrandUniverse program = Set.unions $ map herbrandUniverseRule program

herbrandUniverseRule :: Rule -> Set Term
herbrandUniverseRule (Rule head body) = Set.union (herbrandUniverseFormula head) $ Set.unions $ map herbrandUniverseFormula body

herbrandUniverseFormula :: Formula -> Set Term
herbrandUniverseFormula (Pred _ terms) = Set.fromFoldable $ filterConsts terms
herbrandUniverseFormula (BoxPlus _ formula)= herbrandUniverseFormula formula
herbrandUniverseFormula (BoxMinus _ formula)= herbrandUniverseFormula formula
herbrandUniverseFormula (DiamondPlus _ formula)= herbrandUniverseFormula formula
herbrandUniverseFormula (DiamondMinus _ formula)= herbrandUniverseFormula formula

herbrandPredicates :: Program -> Set (Tuple Predicate Int)
herbrandPredicates program = Set.unions $ map herbrandPredicatesRule program

herbrandPredicatesRule :: Rule -> Set (Tuple Predicate Int)
herbrandPredicatesRule (Rule head body) = Set.union (herbrandPredicatesFormula head) $ Set.unions $ map herbrandPredicatesFormula body

herbrandPredicatesFormula :: Formula -> Set (Tuple Predicate Int)
herbrandPredicatesFormula (Pred predicate terms) = Set.singleton $ Tuple predicate (length terms)
herbrandPredicatesFormula _ = Set.empty

herbrandBase :: Program -> Set Formula
herbrandBase program = Set.unions $ Set.map (\(Tuple pred i) -> Set.fromFoldable $ map (\terms -> Pred pred terms) $ crossProductsOfDimension i $ fromFoldable $ herbrandUniverse program) $ herbrandPredicates program 

crossProductsOfDimension :: forall a. Int -> Array a -> Array (Array a)
crossProductsOfDimension 0 _ = []
crossProductsOfDimension 1 array = map (\element -> [ element ]) array
crossProductsOfDimension i array = map (\element restArray -> element : restArray) array <*> rest
  where
    rest = crossProductsOfDimension (i - 1) array

main :: Effect Unit
main = do
  logShow $ herbrandUniverse jam
  logShow $ herbrandPredicates jam
  logShow $ herbrandBase jam

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