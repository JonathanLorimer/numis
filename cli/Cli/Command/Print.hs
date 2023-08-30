module Cli.Command.Print where

import Options.Applicative
import Cli.Command.Type
import Cli.Command.Parser (sourceParser)
import System.IO (withFile, IOMode (..), stdout)
import System.Exit (die)
import Data.Text.IO (hGetContents, hPutStr)
import Text.Megaparsec (parseMaybe)
import Numis.Language.Parser (ledger)
import Extras.Maybe (handleNothing)
import Cli.Ansi (setErrorColor)
import Numis.Transform.Denormalize (Sign(..))
import Numis.Transform.Table (Table(..), Cell (..))
import Text.Pandoc.Builder (Blocks, simpleTable, plain, text, doc)
import qualified Data.Vector as V
import Data.Functor
import qualified Data.Text as T
import Numis.Transform.Table (toTable)
import Text.Pandoc hiding (Table)

parsePrintCommand :: Parser Command
parsePrintCommand = CommandPrint <$> sourceParser

runPrint :: FilePath -> IO ()
runPrint fp = withFile fp ReadMode \h -> do
  contents <- hGetContents h
  l <- handleNothing (parseMaybe ledger contents) $ setErrorColor >> die "Failed to parse source"
  rst <- runIOorExplode . writeMarkdown (def{writerExtensions = githubMarkdownExtensions}) . doc . toPandocTable . toTable $ l
  hPutStr stdout rst
  
toPandocTable :: Table -> Blocks
toPandocTable (Table{..}) = simpleTable h r
  where
    h :: [Blocks]
    h = plain . text <$> V.toList headers
    
    r :: [[Blocks]]
    r = rows <&> \row ->
          V.toList row <&> \case
            Empty -> mempty
            Item (Positive n) -> plain . text $ "+" <> (T.pack $ show n)
            Item (Negative n) -> plain . text $ "-" <> (T.pack $ show n)
            Description desc -> plain . text $ desc
