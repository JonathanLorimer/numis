module Cli.Command.Parser where

import Options.Applicative
import Cli.Command.Type (OutputFormat(..))

sourceParser :: Parser FilePath
sourceParser = argument str 
   $ metavar "FILEPATH" 
  <> help "path to the numis source file"

outputParser :: Parser (Maybe FilePath)
outputParser = optional 
   $ strOption 
   $ long "output" 
  <> short 'o'
  <> metavar "OUTPUT" 
  <> help "output path for result, if not specified will just print to the command line"

formatParser :: Parser OutputFormat
formatParser = option (maybeReader (\case; "html" -> Just HTML; "md" -> Just MD; "latex" -> Just LaTeX; _ -> Nothing))
   $ long "format" 
  <> short 'f'
  <> value MD
  <> metavar "FORMAT" 
  <> help "the format of the output"
