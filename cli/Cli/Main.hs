module Cli.Main where

import Options.Applicative
import Cli.Command

numisProgDesc :: String
numisProgDesc = "use the numis cli application to run the numis compiler"

numisHeader :: String
numisHeader = "Numis: a domain specific language for financial relations"

main :: IO ()
main = do
    command <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc numisProgDesc <>
                        header numisHeader))
    -- run :: IO ()
    run command
