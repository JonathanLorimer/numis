module Cli.Command.Parser where

import Options.Applicative

sourceParser :: Parser FilePath
sourceParser = argument str $ 
     value "./" 
  <> metavar "FILEPATH" 
  <> help "path to the numis source file"
