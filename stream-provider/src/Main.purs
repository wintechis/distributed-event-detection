module Main where

import Prelude

import Affjax.Node (defaultRequest, printError, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Control.Alt ((<|>))
import Control.Monad.State (StateT, lift)
import Control.Monad.State as State
import Control.Parallel (parSequence_)
import Data.Array (catMaybes, length, mapWithIndex, (!!))
import Data.Array as Array
import Data.DateTime (DateTime, adjust, millisecond, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, toNumber)
import Data.List (List, fromFoldable)
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, stripPrefix, stripSuffix, trim)
import Data.Time.Duration (negateDuration)
import Data.Time.Duration as Duration
import Data.Traversable (sequence)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (nowDateTime)
import Effect.Ref (Ref, modify_, new, read, write)
import Effect.Timer (setInterval)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, briefDesc, execParser, help, helper, idm, info, int, maybeReader, metavar, number, progDesc, some, str, (<**>))

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
  opts <- liftEffect $ execParser optsInfo
  --let values = map (\v -> catMaybes $ fromString <$> split (Pattern ",") v) $ split (Pattern "\n") csv
  --iRef <- new 0
  --_ <- setInterval 1000 $ streamValues options.uri values iRef
  pure unit

--streamValues :: String -> Array (Array Int) -> Ref Int -> Effect Unit
--streamValues scUri valuesArray iRef = launchAff_ do
--  dateTime <- liftEffect nowDateTime
--  let roundedDateTime = fromMaybe dateTime $ adjust (negateDuration $ (\ms -> Duration.Milliseconds ms) $ toNumber $ fromEnum $ millisecond $ time dateTime) dateTime
--  i <- liftEffect $ read iRef
--  liftEffect $ modify_ (\j -> j + 1) iRef
--  case valuesArray !! i of 
--    Nothing -> do
--      liftEffect $ write 0 iRef 
--      liftEffect $ streamValues scUri valuesArray iRef
--    Just values -> parSequence_ $ mapWithIndex (\j v -> sendRequest scUri roundedDateTime j v if j == (length values - 1) then true else false) values

sendRequest :: Options -> DateTime -> StateT Int Aff Unit
sendRequest (Options (SubjectUri subject) (PredicateUri predicate) object (Goal goal) (TimestampUri timestamp) (PoisonUri poison)) datetime = do
  i <- State.get
  res <- lift $ request $ defaultRequest { method = Left POST, url = goal, headers = [ RequestHeader "Content-Type" "text/turtle" ], content = (Just $ RequestBody.string $ "<" <> subject <> "> <" <> predicate <> "> " <> "o" <> " .\n<> <http://ex.org/vocab/timestamp> \"" <> format iso8601Formatter datetime <> "\"^^<http://www.w3.org/2001/XMLSchema#dateTime> ." <> if poison then "\n<> <http://ex.org/vocab/poison> true ." else "") }
  State.modify_ ((+) 1)
  liftEffect $ case res of 
    Left error -> log $ printError error
    Right _ -> pure unit
  
data Options = Options Subject Predicate Object Goal Timestamp Poison
instance showOptions :: Show Options where
  show (Options s p o goal _ _) = show s <> " " <> show p <> " " <> show o <> " -> " <> show goal

data Subject = SubjectUri String
instance showSubject :: Show Subject where
  show (SubjectUri uri) = uri

data Predicate = PredicateUri String
instance showPredicate :: Show Predicate where
  show (PredicateUri uri) = uri

data Object = ObjectIntList (List Int) | ObjectUriList (List String)
instance showObject :: Show Object where
  show (ObjectIntList ints) = show ints
  show (ObjectUriList uris) = show uris

data Goal = Goal String
instance showGoal :: Show Goal where
  show (Goal goal) = goal

data Timestamp = TimestampUri String
instance showTimestampUri :: Show Timestamp where
  show (TimestampUri uri) = uri

data Poison = PoisonUri String
instance showPoisonUri :: Show Poison where
  show (PoisonUri uri) = uri

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
    ( briefDesc
    <> progDesc "Start a Stream Container web server" )

opts :: Parser Options
opts = Options <$> subject <*> predicate <*> argument readObject (metavar "<[URI]|[INT]>") <*> argument (Goal <$> str) (metavar "<GOAL>") <*> argument (TimestampUri <$> str) (metavar "<TIMESTAMP_URI>") <*> argument (PoisonUri <$> str) (metavar "<POISON_URI>")

subject :: Parser Subject
subject = SubjectUri <$> uriParser

predicate :: Parser Predicate
predicate = PredicateUri <$> uriParser

maybeListInt :: String -> Maybe (List Int)
maybeListInt s = do
  r1 <- stripPrefix (Pattern "[") s
  r2 <- stripSuffix (Pattern "]") r1
  let rs = split (Pattern ",") r2
  sequence $ fromFoldable $ fromString <$> rs

maybeListUri :: String -> Maybe (List String)
maybeListUri s = do
  r1 <- stripPrefix (Pattern "[") s
  r2 <- stripSuffix (Pattern "]") r1
  Just $ fromFoldable $ split (Pattern ",") r2

readObject :: ReadM Object
readObject = (ObjectIntList <$> maybeReader maybeListInt) <|> (ObjectUriList <$> maybeReader maybeListUri)

intListParser :: Parser (NonEmptyList Int)
intListParser = some $ argument int $ metavar "INT"

uriParser :: Parser String
uriParser = argument str (metavar "URI")
