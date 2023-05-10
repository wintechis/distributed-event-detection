module CLI where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Int (fromString, toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Time.Duration (Seconds(..))
import Options.Applicative (Parser, ParserInfo, ReadM, briefDesc, eitherReader, help, helper, info, int, long, many, metavar, option, progDesc, short, value, (<**>))
import RDF (Term, namedNode)

data Window = Window {
  membershipResource :: Term,
  hasMemberRelation :: Term,
  contentTimestampRelation :: Term,
  hasPoisonRelation :: Term,
  contentPoisonRelation :: Term,
  start :: Seconds,
  end :: Seconds
}

type Options = {
    port :: Int,
    windows :: List Window
}

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
    ( briefDesc
    <> progDesc "Start a Stream Container web server" )

opts :: Parser Options
opts = ado
  port <- port
  windows <- windows
  in { port, windows: windows }

port :: Parser Int
port = option int (long "port" <> short 'p' <> metavar "PORT" <> value 8080 <> help "The port for the stream container to listen on.")

windows :: Parser (List Window)
windows = many $ option windowReader (long "window" <> short 'w' <> metavar "MEMBERSHIP_RESOURCE MEMBER_RELATION CONTENT_TIMESTAMP_RELATION POISON_RELATION CONTENT_POISON_RELATION START END" <> help "A window for the Stream Container to start with.")

windowReader :: ReadM Window
windowReader = eitherReader parse
  where
    parse :: String -> Either String Window
    parse string = case split (Pattern " ") string of 
      [ membershipResource, memberRelation, contentTimestampRelation, poisonRelation, contentPoisonRelation, startString, endString ] -> case fromString startString of
        Nothing -> Left $ "Start of window \"" <> startString <> "\" is not an integer!"
        Just start -> case fromString endString of 
          Nothing -> Left $ "End of window \"" <> endString <> "\" is not an integer!"
          Just end -> Right $ Window {
            membershipResource: namedNode membershipResource,
            hasMemberRelation: namedNode memberRelation,
            contentTimestampRelation: namedNode contentTimestampRelation,
            hasPoisonRelation: namedNode poisonRelation,
            contentPoisonRelation: namedNode contentPoisonRelation,
            start: Seconds $ toNumber start,
            end: Seconds $ toNumber end
          }
      strArr -> Left $ "Window specifications needs 7 compontens, " <> show (length strArr) <> " were given!"
