module CLI where

import Prelude

import Affjax (URL)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..), split, stripPrefix, toLower)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, briefDesc, eitherReader, help, helper, info, long, many, metavar, option, progDesc, short, (<**>))
import RDF (Term, literalType, namedNode, namedNode', variable)
import RDF.Prefixes (xsd)

data RuleType = And | Box | Diamond
instance showRuleType :: Show RuleType where
  show And = "AND"
  show Box = "BOX"
  show Diamond = "DIAMOND"

data Stream = Stream URL Term Terms
instance showStream :: Show Stream where
  show (Stream uri predicate (Unary var)) = uri <> "[" <> show predicate <> "(" <> show var <> ")" <> "]"
  show (Stream uri predicate (Binary var1 var2)) = uri <> "[" <> show predicate <> "(" <> show var1 <> "," <> show var2 <> ")" <> "]"

data Builtin = LessThanEqual Term Term
instance showBuiltin :: Show Builtin where
  show (LessThanEqual var1 var2) = "less_than_equal(" <> show var1 <> "," <> show var2 <> ")"

data Terms = Unary Term | Binary Term Term

type Options = {
    ruleType :: RuleType,
    sources :: List Stream,
    builtins :: List Builtin,
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
  builtins <- builtins
  goal <- goal
  in { ruleType, sources, builtins, goal }

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

builtins :: Parser (List Builtin)
builtins = many $ option builtinReader (long "builtin" <> short 'b' <> metavar "BUILTIN VARIABLES..." <> help "Builtin predicates that are used instead of stream containers and their variables.")

streamReader :: ReadM Stream
streamReader = eitherReader read 
  where
    read :: String -> Either String Stream
    read str = case split (Pattern " ") str of 
      [ uri, pred, var ] -> Right $ Stream uri (namedNode pred) $ Unary $ termParser var
      [ uri, pred, var1, var2 ] -> Right $ Stream uri (namedNode pred) $ Binary (termParser var1) (termParser var2)
      argSplit -> Left $ "Incorrect arguments for stream given: " <> show argSplit

builtinReader :: ReadM Builtin
builtinReader = eitherReader read
  where
    read :: String -> Either String Builtin
    read str = case split (Pattern " ") (toLower str) of
      [ "less_than_equal", var1, var2 ] -> Right $ LessThanEqual (termParser var1) (termParser var2)
      argSplit -> Left $ "Not a builtin: " <> show argSplit

termParser :: String -> Term
termParser str = case fromString str of 
  Just _ -> literalType str (namedNode' xsd "integer")
  Nothing -> case Number.fromString str of 
    Just _ -> literalType str (namedNode' xsd "decimal")
    Nothing -> case stripPrefix (Pattern "?") str of 
      Nothing -> namedNode str
      Just var -> variable var