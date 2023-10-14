module Cli.Command where

import Data.Text (Text)
import Options.Applicative
import Cli.Command.Type
import Cli.Command.Print (parsePrintCommand, runPrint)

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseCommand :: Parser Command
parseCommand = subparser $
  (command
    "print"
    (info
      (helper <*> parsePrintCommand)
      (fullDesc <> progDesc "parse a numis file and print to stdout as balance sheets")
    )
  )

run :: Command -> IO ()
run = \case
  CommandPrint { filePath, outputFile, format  } -> runPrint filePath outputFile format

