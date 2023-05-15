module CLI where

import Prelude

import Node.Path (FilePath)
import Options.Applicative (Parser, ParserInfo, argument, command, helper, idm, info, metavar, str, subparser, (<**>))

data Options = Options {
    command :: Command,
    programPath :: FilePath
}

instance showOptions :: Show Options where
  show (Options opts) = show opts

data Command = Dot DotOptions
             | DockerCompose DockerComposeOptions
             | ECS ECSOptions

instance showCommand :: Show Command where
  show (Dot _) = "Dot"
  show (DockerCompose _) = "DockerCompose"
  show (ECS _) = "ECS"

data DotOptions = DotOptions

data DockerComposeOptions = DockerComposeOptions

data ECSOptions = ECSOptions

programPathParser :: Parser FilePath
programPathParser = argument str (metavar "PROGRAM_PATH")

optionsParser :: Parser Options
optionsParser = subparser $
  command "dot" (info ((\pp -> Options { command: Dot DotOptions, programPath: pp }) <$> programPathParser) idm) <>
  command "docker-compose" (info ((\pp -> Options { command: DockerCompose DockerComposeOptions, programPath: pp }) <$> programPathParser) idm) <>
  command "ecs" (info ((\pp -> Options { command: ECS ECSOptions, programPath: pp }) <$> programPathParser) idm)

options :: ParserInfo Options
options = info (optionsParser <**> helper) idm