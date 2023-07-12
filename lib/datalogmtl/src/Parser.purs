module DatalogMTL.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..))
import DatalogMTL (Aggregation(..), Formula(..), Interval(..), Predicate(..), Program, Rule(..), Term(..))
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String (char, eof)
import Parsing.String.Basic (alphaNum, letter, upper)
import Parsing.Token (GenLanguageDef(..), LanguageDef, TokenParser, makeTokenParser)

showEitherNaturalOrFloat :: Either Int Number -> String
showEitherNaturalOrFloat (Left i) = show i
showEitherNaturalOrFloat (Right n) = show n

datalogMTLLanguageDef :: LanguageDef
datalogMTLLanguageDef = LanguageDef {
  caseSensitive: true,
  commentEnd: "",
  commentLine: "#",
  commentStart: "",
  identLetter: alphaNum <|> char '_',
  identStart: letter,
  nestedComments: false,
  opLetter: char '-' <|> upper,
  opStart: char ':' <|> char '?' <|> upper,
  reservedNames: [],
  reservedOpNames: [ "?", ":-", "BOXPLUS", "BOXMINUS", "DIAMONDPLUS", "DIAMONDMINUS", "=", "mavg" ]
}

datalogMTLTokenParser :: TokenParser
datalogMTLTokenParser = makeTokenParser datalogMTLLanguageDef

programParser :: Parser String Program
programParser = do
  fs <- many (try aggregationRuleParser <|> ruleParser)
  _ <- eof
  pure $ fs

ruleParser :: Parser String Rule
ruleParser = do
  _ <- datalogMTLTokenParser.whiteSpace
  head <- formulaParser
  _ <- datalogMTLTokenParser.reservedOp ":-"
  body <- datalogMTLTokenParser.commaSep $ formulaParser
  pure $ Rule head $ Array.fromFoldable body

aggregationRuleParser :: Parser String Rule
aggregationRuleParser = do
  _ <- datalogMTLTokenParser.whiteSpace
  head <- formulaParser
  _ <- datalogMTLTokenParser.reservedOp ":-"
  aggVar <- variableParser
  _ <- datalogMTLTokenParser.reservedOp "="
  aggregation <- aggregationParser
  body <- datalogMTLTokenParser.parens $ formulaParser
  pure $ AggrRule head aggregation aggVar body

aggregationParser :: Parser String Aggregation
aggregationParser = datalogMTLTokenParser.reservedOp "mavg" >>= \_ -> pure Average

formulaParser :: Parser String Formula
formulaParser = do
  guard true
  try boxPlusParser <|> try boxMinusParser <|> try diamondPlusParser <|> try diamondMinusParser <|> predParser

predParser :: Parser String Formula
predParser = do
  p <- datalogMTLTokenParser.identifier
  ts <- datalogMTLTokenParser.parens $ datalogMTLTokenParser.commaSep (variableParser <|> numberParser <|> constantParser)
  pure $ Pred (Predicate p) $ Array.fromFoldable ts

variableParser :: Parser String Term
variableParser = do
  _ <- datalogMTLTokenParser.reservedOp "?"
  var <- datalogMTLTokenParser.identifier
  pure $ Variable var

numberParser :: Parser String Term
numberParser = do
  num <- datalogMTLTokenParser.naturalOrFloat
  pure $ Constant $ showEitherNaturalOrFloat num

constantParser :: Parser String Term
constantParser = do
  const <- datalogMTLTokenParser.identifier
  pure $ Constant const

intervalParser :: Parser String Interval
intervalParser = do
  start <- datalogMTLTokenParser.integer
  _ <- datalogMTLTokenParser.comma
  end <- datalogMTLTokenParser.integer
  pure $ Interval start end

boxPlusParser :: Parser String Formula
boxPlusParser = do
  _ <- datalogMTLTokenParser.reservedOp "BOXPLUS"
  i <- datalogMTLTokenParser.brackets intervalParser
  f <- formulaParser
  pure $ BoxPlus i f

boxMinusParser :: Parser String Formula
boxMinusParser = do
  _ <- datalogMTLTokenParser.reservedOp "BOXMINUS"
  i <- datalogMTLTokenParser.brackets intervalParser
  f <- formulaParser
  pure $ BoxMinus i f

diamondPlusParser :: Parser String Formula
diamondPlusParser = do
  _ <- datalogMTLTokenParser.reservedOp "DIAMONDPLUS"
  i <- datalogMTLTokenParser.brackets intervalParser
  f <- formulaParser
  pure $ DiamondPlus i f

diamondMinusParser :: Parser String Formula
diamondMinusParser = do
  _ <- datalogMTLTokenParser.reservedOp "DIAMONDMINUS"
  i <- datalogMTLTokenParser.brackets intervalParser
  f <- formulaParser
  pure $ DiamondMinus i f

testString :: String
testString = "BOXMINUS[3,5] speed(?x, 10,   ?daniel ,?car,123) :-  BOXPLUS[4, 7] DIAMONDPLUS[0,0] car(?daniel), speed( ?x) , BOXMINUS[1123,23123] bla(123, ?beep)\n  BOXPLUS[3,5] speed(?x, 10,   ?daniel ,?car , 124) :-  DIAMONDPLUS[0,0] car(?daniel), speed( ?x) , BOXMINUS[1123,23123] bla(123, ?beep)"

testAgg :: String
testAgg = "avg_speed_5(?car, ?avg_speed) :-  ?avg_speed = mavg(DIAMONDMINUS[0,5] speed(?car, ?speed))"

testFormula :: String
testFormula = "BOXPLUS[3,5] DIAMONDMINUS[-2,4] speed(?x, 10,   ?daniel ,?car,123)"

main :: Effect Unit
main = do
  program <- readTextFile UTF8 "../../plan-generator/program.dmtl"
  logShow $ runParser program programParser