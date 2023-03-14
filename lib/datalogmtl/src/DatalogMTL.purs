module DatalogMTL where

import Prelude

import Data.Array (concatMap, filter, fromFoldable, length, mapMaybe, nub, (:))
import Data.Array as Array
import Data.Array.NonEmpty (toArray)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.String.Gen (genAlphaLowercaseString)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (logShow)
import Test.QuickCheck (class Arbitrary, arbitrary, randomSeed)
import Test.QuickCheck.Gen (Gen, arrayOf, arrayOf1, chooseInt, elements, resize, runGen, suchThat)

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
instance arbitraryTerm :: Arbitrary Term where
  arbitrary = do
    name <- resize 5 genAlphaLowercaseString
    varOrConst <- elements $ NonEmptyArray [ Variable, Constant ]
    pure $ varOrConst name

data Predicate = Predicate String
instance showPredicate :: Show Predicate where
  show (Predicate name) = name
derive instance eqPredicate :: Eq Predicate
derive instance ordPredicate :: Ord Predicate
instance arbitraryPredicate :: Arbitrary Predicate where
  arbitrary = do
    name <- resize 7 genAlphaLowercaseString
    pure $ Predicate name

data Formula = Pred Predicate (Array Term) | BoxPlus Interval Formula | BoxMinus Interval Formula | DiamondPlus Interval Formula | DiamondMinus Interval Formula
instance showFormula :: Show Formula where
  show (Pred predicate terms) = show predicate <> "(" <> joinWith ", " (show <$> terms) <> ")"
  show (BoxPlus interval formula) = "□+_" <> show interval <> " " <> show formula
  show (BoxMinus interval formula) = "□-_" <> show interval <> " " <> show formula
  show (DiamondPlus interval formula) = "◇+_" <> show interval <> " " <> show formula
  show (DiamondMinus interval formula) = "◇-_" <> show interval <> " " <> show formula
derive instance eqFormula :: Eq Formula
derive instance ordFormula :: Ord Formula
instance arbitraryFormula :: Arbitrary Formula where
  arbitrary = do
    constructor <- chooseInt 0 4
    case constructor of 
      1 -> do
        intervalStart <- chooseInt 0 20
        intervalEnd <- suchThat (chooseInt 0 20) (\end -> end >= intervalStart)
        formula <- arbitrary
        pure $ BoxPlus (Interval intervalStart intervalEnd) formula
      2 -> do
        intervalStart <- chooseInt 0 20
        intervalEnd <- suchThat (chooseInt 0 20) (\end -> end >= intervalStart)
        formula <- arbitrary
        pure $ BoxMinus (Interval intervalStart intervalEnd) formula
      3 -> do
        intervalStart <- chooseInt 0 20
        intervalEnd <- suchThat (chooseInt 0 20) (\end -> end >= intervalStart)
        formula <- arbitrary
        pure $ DiamondPlus (Interval intervalStart intervalEnd) formula
      4 -> do
        intervalStart <- chooseInt 0 20
        intervalEnd <- suchThat (chooseInt 0 20) (\end -> end >= intervalStart)
        formula <- arbitrary
        pure $ DiamondMinus (Interval intervalStart intervalEnd) formula
      _ -> do
        pred <- arbitrary
        terms <- (resize 3 <$> arrayOf) arbitrary
        pure $ Pred pred terms

data Rule = Rule Formula (Array Formula)
instance showRule :: Show Rule where
  show (Rule head body) = show head <> " ← " <> joinWith " ∧ " (show <$> body)
derive instance eqRule :: Eq Rule
derive instance ordRule :: Ord Rule
instance arbitraryRule :: Arbitrary Rule where
  arbitrary = do
    body <- toArray <$> (arrayOf1 $ suchThat (arbitrary :: Gen Formula) forwardPropagatingBody)
    head <- suchThat (suchThat (arbitrary :: Gen Formula) forwardPropagatingHead) $ isSafe body
    pure $ Rule head body

type Program = Array Rule

chooseTerms :: Gen (Set Term)
chooseTerms = Set.fromFoldable <$> arrayOf1 arbitrary

choosePredicates :: Gen (Set (Tuple Predicate Int))
choosePredicates = Set.fromFoldable <$> arrayOf1 do
  pred <- arbitrary
  arity <- chooseInt 0 4
  pure $ Tuple pred arity

herbrandBaseFromTermsAndPredicates :: Set Term -> Set (Tuple Predicate Int) -> Set Formula
herbrandBaseFromTermsAndPredicates universe predicates = Set.unions $ Set.map (\(Tuple pred i) -> Set.fromFoldable $ map (\terms -> Pred pred $ filterConsts terms) $ crossProductsOfDimension i $ fromFoldable universe) predicates

crossProductsOfDimension :: forall a. Int -> Array a -> Array (Array a)
crossProductsOfDimension 0 _ = []
crossProductsOfDimension 1 array = map (\element -> [ element ]) array
crossProductsOfDimension i array = map (\element restArray -> element : restArray) array <*> rest
  where
    rest = crossProductsOfDimension (i - 1) array

forwardPropagatingHead :: Formula -> Boolean
forwardPropagatingHead (Pred _ _) = true
forwardPropagatingHead (BoxPlus _ rest) = forwardPropagatingHead rest
forwardPropagatingHead (BoxMinus _ _) = false
forwardPropagatingHead (DiamondPlus _ rest) = forwardPropagatingHead rest
forwardPropagatingHead (DiamondMinus _ rest) = forwardPropagatingHead rest

forwardPropagatingBody :: Formula -> Boolean
forwardPropagatingBody (Pred _ _) = true
forwardPropagatingBody (BoxPlus _ _) = false
forwardPropagatingBody (BoxMinus _ rest) = forwardPropagatingBody rest
forwardPropagatingBody (DiamondPlus _ _) = false
forwardPropagatingBody (DiamondMinus _ rest) = forwardPropagatingBody rest

isSafe :: Array Formula -> Formula -> Boolean
isSafe body head = (Set.size $ Set.difference (getVariables head) (Set.unions $ getVariables <$> body)) == 0

getVariables :: Formula -> Set Term
getVariables (Pred _ terms) = Set.fromFoldable $ filterVars terms
getVariables (BoxPlus _ rest) = getVariables rest
getVariables (BoxMinus _ rest) = getVariables rest
getVariables (DiamondPlus _ rest) = getVariables rest
getVariables (DiamondMinus _ rest) = getVariables rest

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
normalFormula (Pred predicate terms) = Tuple [] (Tuple predicate terms)
normalFormula formula@(BoxPlus (Interval start end) (Pred (Predicate predicate) terms)) = Tuple [ Rule (Pred newPred terms) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (BoxPlus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Pred (newPred pred) ts) [ BoxPlus (Interval start end) (Pred pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("boxPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(BoxMinus (Interval start end) (Pred (Predicate predicate) terms)) = Tuple [ Rule (Pred newPred (filterVars terms)) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("boxMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (BoxMinus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Pred (newPred pred) ts) [ BoxMinus (Interval start end) (Pred pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("boxMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(DiamondPlus (Interval start end) (Pred (Predicate predicate) terms)) = Tuple [ Rule (Pred newPred (filterVars terms)) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("diamondPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (DiamondPlus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Pred (newPred pred) ts) [ DiamondPlus (Interval start end) (Pred pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("diamondPlus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))
normalFormula formula@(DiamondMinus (Interval start end) (Pred (Predicate predicate) terms)) = Tuple [ Rule (Pred newPred terms) [ formula ] ] (Tuple newPred (filterVars terms))
  where
    newPred = Predicate ("diamondMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts terms)))
normalFormula (DiamondMinus (Interval start end) formula') = case normalFormula formula' of
  Tuple prog (Tuple pred ts) -> Tuple (Rule (Pred (newPred pred) ts) [ DiamondMinus (Interval start end) (Pred pred ts) ] : prog) (Tuple (newPred pred) (filterVars ts))
    where
      newPred (Predicate predicate) = Predicate ("diamondMinus_" <> show start <> "_" <> show end <> "_" <> predicate <> joinWith "_" (map show (filterConsts ts)))

normalRule :: Rule -> Program
normalRule rule@(Rule head body) = if isNormalRule body then [ rule ] else [ Rule head (map (\(Tuple _ (Tuple predicate terms)) -> Pred predicate terms) tuples) ] <> concatMap (\(Tuple program _) -> program) tuples
  where
    tuples :: Array (Tuple Program (Tuple Predicate (Array Term)))
    tuples = map normalFormula body

isNormalRule :: Array Formula -> Boolean
isNormalRule body = if length body == 1 then (length (filter (\d -> d > 1) (map formulaDepth body))) == 0 else (length (filter (\d -> d > 0) (map formulaDepth body))) == 0

formulaDepth :: Formula -> Int
formulaDepth (Pred _ _) = 0
formulaDepth (BoxPlus _ formula) = formulaDepth formula + 1
formulaDepth (BoxMinus _ formula) = formulaDepth formula + 1
formulaDepth (DiamondPlus _ formula) = formulaDepth formula + 1
formulaDepth (DiamondMinus _ formula) = formulaDepth formula + 1

normalForm :: Program -> Program
normalForm program = nub $ concatMap normalRule program

formulaPredicate :: Formula -> Predicate
formulaPredicate (Pred predicate _) = predicate
formulaPredicate (BoxPlus _ formula) = formulaPredicate formula
formulaPredicate (BoxMinus _ formula) = formulaPredicate formula
formulaPredicate (DiamondPlus _ formula) = formulaPredicate formula
formulaPredicate (DiamondMinus _ formula) = formulaPredicate formula

rulePredicates :: Rule -> Set Predicate
rulePredicates (Rule head body) = Set.insert (formulaPredicate head) (Set.fromFoldable $ map formulaPredicate body)

programPredicates :: Program -> Set Predicate
programPredicates program = Set.unions $ map rulePredicates program

getIntervallsForPredicates :: Program -> Map Predicate (Set Interval)
getIntervallsForPredicates program = Map.fromFoldable $ Set.map (\p -> Tuple p (getIntervallsForPredicate p)) (programPredicates program)
  where
    getIntervallsForPredicate :: Predicate -> Set Interval
    getIntervallsForPredicate predicate = Set.fromFoldable $ concatMap (\(Rule _ body) -> concatMap getIntervallsForPredicateFormula body <> mapMaybe getZeroIntervalForRule body) program
      where
        getZeroIntervalForRule :: Formula -> Maybe Interval
        getZeroIntervalForRule (Pred predicate' _) = if predicate == predicate' then Just (Interval 0 0) else Nothing
        getZeroIntervalForRule _ = Nothing

        getIntervallsForPredicateFormula :: Formula -> Array Interval
        getIntervallsForPredicateFormula (Pred _ _) = []
        getIntervallsForPredicateFormula (BoxPlus interval (Pred predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula (BoxMinus (Interval start end) (Pred predicate' _)) = if predicate == predicate' then [ (Interval (negate start) (negate end)) ] else []
        getIntervallsForPredicateFormula (DiamondPlus interval (Pred predicate' _)) = if predicate == predicate' then [ interval ] else []
        getIntervallsForPredicateFormula (DiamondMinus (Interval start end) (Pred predicate' _)) = if predicate == predicate' then [ Interval (negate start) (negate end) ] else []
        getIntervallsForPredicateFormula _ = []

showIntervalForPredicates :: Map Predicate (Set Interval) -> String
showIntervalForPredicates fiMap = joinWith "\n" $ map (\(Tuple predicate intervals) -> show predicate <> ":\t" <> (joinWith ", " $ map show $ Array.fromFoldable intervals)) $ Map.toUnfoldable fiMap

getPredicatesForRule :: Rule -> Array Predicate
getPredicatesForRule (Rule head body) = getPredicateForFormula head : (getPredicateForFormula <$> body)

getPredicateForFormula :: Formula -> Predicate 
getPredicateForFormula (Pred pred _) = pred
getPredicateForFormula (BoxPlus _ formula) = getPredicateForFormula formula
getPredicateForFormula (BoxMinus _ formula) = getPredicateForFormula formula
getPredicateForFormula (DiamondPlus _ formula) = getPredicateForFormula formula
getPredicateForFormula (DiamondMinus _ formula) = getPredicateForFormula formula

main :: Effect Unit
main = do
  seed <- randomSeed
  let terms = fst $ runGen chooseTerms { newSeed: seed, size: 10}
  logShow terms
  let predicates = fst $ runGen choosePredicates { newSeed: seed, size: 10}
  logShow predicates
  --let base = herbrandBaseFromTermsAndPredicates terms predicates
  --logShow base