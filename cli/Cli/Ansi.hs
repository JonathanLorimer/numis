module Cli.Ansi where

import qualified System.Console.ANSI as ANSI

setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [-- color to set
                             ANSI.SetColor
                             -- wherther foreground / background should be affected
                             ANSI.Foreground
                             -- use the "vivid" color versus the muted color
                             ANSI.Vivid
                             -- use red
                             ANSI.Red
                            ]  
