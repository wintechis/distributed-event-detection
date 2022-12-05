module Main where

import Prelude

import Affjax.Node (defaultRequest, printError, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Data.Array (catMaybes, (!!))
import Data.DateTime (adjust, millisecond, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, toNumber)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Time.Duration (negateDuration)
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read, write)
import Effect.Timer (setInterval)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Options.Applicative (Parser, ParserInfo, argument, briefDesc, execParser, help, helper, info, metavar, progDesc, str, (<**>))

iso8601Formatter :: Formatter
iso8601Formatter = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder "Z"
  ]

main :: Effect Unit
main = do
  options <-liftEffect $ execParser optsInfo
  csv <- readTextFile UTF8 options.csvFileName
  let values = catMaybes $ fromString <$> split (Pattern "\n") csv
  iRef <- new 0
  _ <- setInterval 1000 $ streamValues options.uri values iRef
  pure unit

streamValues :: String -> Array Int -> Ref Int -> Effect Unit
streamValues scUri values iRef = launchAff_ do
  dateTime <- liftEffect nowDateTime
  let roundedDateTime = fromMaybe dateTime $ adjust (negateDuration $ (\ms -> Duration.Milliseconds ms) $ toNumber $ fromEnum $ millisecond $ time dateTime) dateTime
  i <- liftEffect $ read iRef
  liftEffect $ modify_ (\j -> j + 1) iRef
  case values !! i of 
    Nothing -> do
      liftEffect $ write 0 iRef 
    Just value -> do
      res <- request $ defaultRequest { method = Left POST, url = scUri, headers = [ RequestHeader "Content-Type" "text/turtle" ], content = (Just $ RequestBody.string $ "<http://ex.org/cars/1> <http://ex.org/vocab/speed> " <> show value <> " .\n<> <http://ex.org/vocab/timestamp> \"" <> format iso8601Formatter roundedDateTime <> "\"^^<http://www.w3.org/2001/XMLSchema#dateTime> .") }
      liftEffect $ case res of 
        Left error -> log $ printError error
        Right _ -> pure unit

type Options = {
    csvFileName :: String,
    uri :: String
}

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
    ( briefDesc
    <> progDesc "Start a Stream Container web server" )

opts :: Parser Options
opts = ado
  csvFileName <- csvFileName
  uri <- uri
  in { csvFileName, uri }

csvFileName :: Parser String
csvFileName = argument str (metavar "CSV_FILE" <> help "The CSV file to get the values from.")

uri :: Parser String
uri = argument str (metavar "URI" <> help "URI of stream container to stream to.")
