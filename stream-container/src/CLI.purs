module CLI where

import Prelude

import Data.Array (head, length, tail)
import Data.Either (Either(..), hush, note)
import Data.Int (fromString, toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, stripPrefix, stripSuffix, trim)
import Data.String.NonEmpty (nes)
import Data.These (These(..))
import Data.Time.Duration (Seconds(..))
import Data.Traversable (sequence)
import Options.Applicative (Parser, ParserInfo, ReadM, briefDesc, eitherReader, help, helper, info, long, many, maybeReader, metavar, option, progDesc, short, str, value, (<**>))
import Parsing (runParser)
import RDF (Term, literalType, namedNode, namedNode')
import RDF.Prefixes (xsd)
import Type.Proxy (Proxy(..))
import URI (Authority(..), Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), Port, Query, URI(..), UserInfo)
import URI.Host as Host
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.Scheme.Common (http)
import URI.URI (URIOptions)

data Window = Window {
  membershipResource :: Term,
  hasMemberRelation :: Term,
  contentTimestampRelation :: Term,
  hasPoisonRelation :: Term,
  contentPoisonRelation :: Term,
  start :: Seconds,
  end :: Seconds
}

data DataProvider = DataProvider Term (Array Term)

type SCURI = URI UserInfo (HostPortPair Host Port) Path HierPath Query Fragment

uriOptions ∷ Record (URIOptions UserInfo (HostPortPair Host Port) Path HierPath Query Fragment)
uriOptions =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }

type Options = {
  uri :: SCURI,
  predicate :: Maybe Term,
  dataProviders :: List DataProvider,
  windows :: List Window
}

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
    ( briefDesc
    <> progDesc "Start a Stream Container web server" )

opts :: Parser Options
opts = ado
  hostname <- hostname
  port <- port
  predicate <- predicate
  dataProviders <- dataProviders
  windows <- windows
  in { uri: URI http (HierarchicalPartAuth (Authority Nothing $ Just $ Both hostname port) $ Path []) Nothing Nothing, predicate, dataProviders, windows }

hostname :: Parser Host
hostname = option (maybeReader (\s -> hush $ runParser s Host.parser )) (long "hostname" <> short 'H' <> metavar "HOSTNAME" <> value (NameAddress $ RegName.fromString (nes $ (Proxy :: Proxy "localhost"))) <> help "The hostname under which the stream container shall run.")

port :: Parser Port
port = option (maybeReader (\s -> fromString s >>= Port.fromInt)) (long "port" <> short 'P' <> metavar "PORT" <> value (Port.unsafeFromInt 8080) <> help "The port for the stream container to listen on.")

predicate :: Parser (Maybe Term)
predicate = option (Just <$> namedNode <$> str) (long "predicate" <> short 'p' <> metavar "URI" <> value Nothing <> help "Predicate that is assigned to the stream container (purely informative).")

dataProviders :: Parser (List DataProvider)
dataProviders = many $ option dataProviderReader (long "data-provider" <> short 'd' <> metavar "SUBJECT OBJECT_LIST" <> help "Subject and list of objects that the stream container should simulate.")

dataProviderReader :: ReadM DataProvider
dataProviderReader = eitherReader parse 
  where
    parse :: String -> Either String DataProvider
    parse s = note "Not able to parse data provider!" do
      subject <- head $ split (Pattern " ") s
      objectList <- joinWith "" <$> (tail $ split (Pattern " ") s)
      oL <- stripPrefix (Pattern "[") objectList
      oL' <- stripSuffix (Pattern "]") oL
      let objects = trim <$> split (Pattern ",") oL'
      let objectTerms = case sequence (map fromString objects) of 
            Nothing -> map namedNode objects
            Just _ -> map (\i -> literalType i (namedNode' xsd "integer")) objects
      Just $ DataProvider (namedNode subject) objectTerms

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
