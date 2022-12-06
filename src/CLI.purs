module CLI where

import Prelude

import Affjax (URL)
import Data.Either (Either(..))
import Data.List (List)
import Data.String (Pattern(..), split, toLower)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, briefDesc, eitherReader, help, helper, info, long, many, metavar, option, progDesc, short, (<**>))

data RuleType = And | Box | Diamond
instance showRuleType :: Show RuleType where
  show And = "AND"
  show Box = "BOX"
  show Diamond = "DIAMOND"

data Stream = Stream URL URL Variables
instance showStream :: Show Stream where
  show (Stream uri predicate (Unary var)) = uri <> "[" <> predicate <> "(?" <> var <> ")" <> "]"
  show (Stream uri predicate (Binary var1 var2)) = uri <> "[" <> predicate <> "(?" <> var1 <> ", " <> var2 <> ")" <> "]"

data Variables = Unary String | Binary String String

type Options = {
    ruleType :: RuleType,
    sources :: List Stream,
    goal :: Stream
}

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
    ( briefDesc
    <> progDesc "Start a Stream Processing Agent" )

opts :: Parser Options
opts = ado
  ruleType <- ruleType
  sources <- sources
  goal <- goal
  in { ruleType, sources, goal }

ruleType :: Parser RuleType
ruleType = argument ruleTypeReader (metavar "RULE_TYPE" <> help "The type of rule for this agent to process. Possible are AND, BOX, and DIAMOND.")

ruleTypeReader :: ReadM RuleType
ruleTypeReader = eitherReader read
  where
    read :: String -> Either String RuleType
    read str = case toLower str of 
      "and" -> Right And 
      "box" -> Right Box
      "diamond" -> Right Diamond
      _ -> Left $ "\"" <> str <> "\" does not match a valid rule type! Possible are AND, BOX, and DIAMOND."

sources :: Parser (List Stream)
sources = many $ option streamReader (long "source" <> short 's' <> metavar "URI VARIABLES..." <> help "The stream container sources and their varibales.")

goal :: Parser Stream
goal = option streamReader (long "goal" <> short 'g' <> metavar "URI VARIABLES..." <> help "The stream container goal and their varibales.")

streamReader :: ReadM Stream
streamReader = eitherReader read 
  where
    read :: String -> Either String Stream
    read str = case split (Pattern " ") str of 
      [ uri, pred, var ] -> Right $ Stream uri pred $ Unary var
      [ uri, pred, var1, var2 ] -> Right $ Stream uri pred $ Binary var1 var2
      argSplit -> Left $ "Incorrect arguments for stream given: " <> show argSplit
