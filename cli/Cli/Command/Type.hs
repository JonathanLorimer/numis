module Cli.Command.Type where

data Command = CommandPrint { filePath :: FilePath }
  deriving Show
