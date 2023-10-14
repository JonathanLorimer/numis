module Cli.Command.Type where

data OutputFormat = HTML | MD | LaTeX deriving (Show)

data Command = CommandPrint
  { filePath :: FilePath
  , outputFile :: Maybe FilePath
  , format :: OutputFormat
  }
  deriving (Show)
