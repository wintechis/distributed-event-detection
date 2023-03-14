module Test.Main where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import DatalogMTL (Formula(..), Interval(..), Predicate(..), Program, Term(..), normalForm)
import DatalogMTL.Parser (formulaParser)
import DatalogMTL.Semantics (crossProductsOfDimension)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Parsing (runParser)
import Test.Spec (describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

formulaParserTests :: Array (Tuple String Formula)
formulaParserTests = [
  Tuple "testPredicate(10, ?testVar, testLiteral)" (Pred (Predicate "testPredicate") [ Constant "10", Variable "testVar", Constant "testLiteral" ]),
  Tuple "BOXPLUS [-3,5] testPredicate(10, ?testVar, testLiteral)" (BoxPlus (Interval (-3) 5) $ Pred (Predicate "testPredicate") [ Constant "10", Variable "testVar", Constant "testLiteral" ]),
  Tuple "DIAMONDMINUS [-312,523] BOXMINUS [203, 1324] testPredicate(10, ?testVar, testLiteral, ?nextVar)" (DiamondMinus (Interval (-312) 523) $ BoxMinus (Interval 203 1324) $ Pred (Predicate "testPredicate") [ Constant "10", Variable "testVar", Constant "testLiteral", Variable "nextVar" ])
]

normalFormTests :: Array (Tuple Program Program)
normalFormTests = [
--  Tuple [
--    Rule  
--  ] [
--
--  ]
]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "DatalogMTL" do
    it "normalForm" $ quickCheck \program -> length (normalForm program) <= length program
  describe "DatalogMTL.Parser" do
    parallel $ sequence_ $ map (\(Tuple string formula) -> it ("formulaParser: " <> string) $ shouldEqual (runParser string formulaParser) (Right formula)) formulaParserTests
  describe "DatalogMTL.Reasoning" do
    it "crossProductsOfDimension" do
      shouldEqual (crossProductsOfDimension 0 [ "a" ]) []
      shouldEqual (crossProductsOfDimension 1 [ "a" ]) [ [ "a" ] ]
      shouldEqual (crossProductsOfDimension 1 [ "a", "b" ]) [ [ "a" ], [ "b" ] ]
      shouldEqual (crossProductsOfDimension 1 [ "a", "b", "c" ]) [ [ "a" ], [ "b" ], [ "c" ] ]
      shouldEqual (crossProductsOfDimension 2 [ "a", "b" ]) [ [ "a", "a" ], [ "a", "b" ], ["b", "a" ], ["b", "b" ] ]
      shouldEqual (crossProductsOfDimension 2 [ "a", "b", "c" ]) [
        [ "a", "a" ], [ "a", "b" ], [ "a", "c" ],
        [ "b", "a" ], [ "b", "b" ], [ "b", "c" ],
        [ "c", "a" ], [ "c", "b" ], [ "c", "c" ]
      ]
      shouldEqual (crossProductsOfDimension 3 [ "a" ]) [ [ "a", "a", "a" ] ]
      shouldEqual (crossProductsOfDimension 3 [ "a", "b" ]) [ [ "a", "a", "a" ], ["a", "a", "b" ], ["a", "b", "a" ], ["a", "b", "b" ], ["b", "a", "a" ], ["b", "a", "b" ], ["b", "b", "a" ], ["b", "b", "b" ] ]
      shouldEqual (crossProductsOfDimension 3 [ "a", "b", "c" ]) [
        [ "a", "a", "a" ], [ "a", "a", "b" ], [ "a", "a", "c" ], ["a", "b", "a" ], [ "a", "b", "b" ], ["a", "b", "c" ], ["a", "c", "a" ], ["a", "c", "b" ], ["a", "c", "c" ],
        ["b", "a", "a" ], [ "b", "a", "b" ], ["b", "a", "c" ], ["b", "b", "a" ], ["b", "b", "b" ], ["b", "b", "c" ], ["b", "c", "a" ], ["b", "c", "b" ], ["b", "c", "c" ],
        ["c", "a", "a" ], ["c", "a", "b" ], ["c", "a", "c" ], ["c", "b", "a" ], ["c", "b", "b" ], ["c", "b", "c" ], ["c", "c", "a" ], ["c", "c", "b" ], ["c", "c", "c" ]
      ]
