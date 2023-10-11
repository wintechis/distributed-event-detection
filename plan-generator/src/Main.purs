module Main where

import Prelude

import Backends.DockerCompose (writeCompose)
import CLI (Command(..), Options(..), options)
import Data.Either (Either(..))
import DatalogMTL.Parser (programParser)
import Effect (Effect)
import Effect.Console (error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Options.Applicative (execParser)
import Parsing (parseErrorMessage, runParser)

main :: Effect Unit
main = do
  (Options opts) <- execParser options
  programString <- readTextFile UTF8 opts.programPath
  let eitherErrorProgram = runParser programString programParser
  case eitherErrorProgram of 
    Left err -> error $ parseErrorMessage err
    Right program ->
      case opts.command of
        (DockerCompose _) -> writeCompose  program 
        _ -> error "Not implemented"