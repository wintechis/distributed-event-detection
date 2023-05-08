module MainNew
  ( Plan(..)
  , ReasoningNode(..)
  , ReasoningType(..)
  , StreamNode(..)
  , Window(..)
  , createPlan
  , getTermsWithWindows
  , getTermsWithWindowsRule
  )
  where

import Prelude

import Data.Array (catMaybes, concat, filter, fromFoldable, groupAllBy)
import Data.Array as Array
import Data.Array.NonEmpty (head, toArray)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import DatalogMTL (Formula(..), Interval(..), Predicate(..), Program, Rule(..))

data StreamNode = StreamNode Predicate

derive instance eqStreamNode :: Eq StreamNode

derive instance ordStreamNode :: Ord StreamNode

instance showStreamNode :: Show StreamNode where
  show (StreamNode pred) = show pred

data Window = Window StreamNode Int Int 

instance showWindow :: Show Window where
  show (Window streamNode start end) = show streamNode <> "[" <> show start <> ", " <> show end <> "]"

derive instance eqWindow :: Eq Window

derive instance ordWindow :: Ord Window

data ReasoningNode = ReasoningNode ReasoningType (Array Window) StreamNode

instance showReasoning :: Show ReasoningNode where
  show (ReasoningNode reasoningType windows streamNode) = show reasoningType <> ": " <> show windows <> " -> " <> show streamNode

data ReasoningType = Box | Diamond | And | Agg

instance showReasoningType :: Show ReasoningType where
  show Box = "□"
  show Diamond = "◇"
  show And = "∧"
  show Agg = "A"

data Plan = Plan (Array ReasoningNode) (Array StreamNode) (Map StreamNode (Set Window))

instance showPlan :: Show Plan where
  show (Plan rNodes sNodes wToS) = show rNodes <> "\n" <> show sNodes <> "\n" <> show wToS

createPlan :: Program -> Plan
createPlan program = Plan (catMaybes $ map ruleToReasoningNode program) (fromFoldable $ fst <$> Map.values streamNodesMap) (Map.fromFoldable $ map (\(Tuple sn ws) -> Tuple sn (Set.fromFoldable ws)) $ Map.values streamNodesMap)
  where
    ruleToReasoningNode :: Rule -> Maybe ReasoningNode
    ruleToReasoningNode (Rule (Pred predH _) [ BoxPlus (Interval start end) (Pred predB _) ]) = Just $ ReasoningNode Box [ (getWindow predB start end) ] $ StreamNode predH
    ruleToReasoningNode (Rule (Pred predH _) [ BoxMinus (Interval start end) (Pred predB _) ]) = Just $ ReasoningNode Box [ (getWindow predB (negate start) (negate end)) ] $ StreamNode predH
    ruleToReasoningNode (Rule (Pred predH _) [ DiamondPlus (Interval start end) (Pred predB _) ]) = Just $ ReasoningNode Diamond [ (getWindow predB start end) ] $ StreamNode predH
    ruleToReasoningNode (Rule (Pred predH _) [ DiamondMinus (Interval start end) (Pred predB _) ]) = Just $ ReasoningNode Diamond [ (getWindow predB (negate start) (negate end)) ] $ StreamNode predH
    ruleToReasoningNode (Rule (Pred predH _) conj) = Just $ ReasoningNode And (catMaybes $ map getWindowConj conj) $ StreamNode predH
    ruleToReasoningNode (AggrRule (Pred predH _) _ _ (Pred predB _)) = Just $ ReasoningNode Agg [ getWindow predB 0 0 ] $ StreamNode predH
    ruleToReasoningNode _ = Nothing
    getWindowConj :: Formula -> Maybe Window
    getWindowConj (Pred p _) = Just $ getWindow p 0 0
    getWindowConj _ = Nothing
    getWindow :: Predicate -> Int -> Int -> Window
    getWindow predicate start end = fromMaybe (Window (StreamNode (Predicate "error")) 0 0) $ Array.head $ filter (\(Window _ s e) -> start == s && end == e) $ fromMaybe [] $ snd <$> Map.lookup predicate streamNodesMap
    streamNodesMap :: Map Predicate (Tuple StreamNode (Array Window))
    streamNodesMap = Map.fromFoldable $ map (\group -> Tuple (fst $ head group) (Tuple (StreamNode (fst $ head group)) (map ((\(Tuple _ (Tuple start end)) -> Window (StreamNode (fst $ head group)) start end)) $ toArray group))) $ groupAllBy (\(Tuple p1 _) (Tuple p2 _) -> compare p1 p2) $ getTermsWithWindows program

getTermsWithWindows :: Program -> Array (Tuple Predicate (Tuple Int Int))
getTermsWithWindows program = concat $ map getTermsWithWindowsRule program

getTermsWithWindowsRule :: Rule -> Array (Tuple Predicate (Tuple Int Int))
getTermsWithWindowsRule (Rule head body) = getTermsWithWindowsFormula head <> (concat $ map getTermsWithWindowsFormula body)
getTermsWithWindowsRule (AggrRule head _ _ body) = getTermsWithWindowsFormula head <> getTermsWithWindowsFormula body

getTermsWithWindowsFormula :: Formula -> Array (Tuple Predicate (Tuple Int Int))
getTermsWithWindowsFormula (Pred predicate _) = [Tuple predicate $ Tuple 0 0]
getTermsWithWindowsFormula (BoxPlus (Interval start end) (Pred predicate _)) = [Tuple predicate $ Tuple start end]
getTermsWithWindowsFormula (BoxMinus (Interval start end) (Pred predicate _)) = [Tuple predicate $ Tuple (negate start) (negate end)]
getTermsWithWindowsFormula (DiamondPlus (Interval start end) (Pred predicate _)) = [Tuple predicate $ Tuple start end]
getTermsWithWindowsFormula (DiamondMinus (Interval start end) (Pred predicate _)) = [Tuple predicate $ Tuple (negate start) (negate end)]
getTermsWithWindowsFormula _ = []