module Cli.Command.Print where

import Cli.Ansi (setErrorColor)
import Cli.Command.Parser
import Cli.Command.Type
import Control.Monad.IO.Class
import Data.Functor
import Data.List (singleton, transpose)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Extras.Maybe (handleNothing)
import Numis.Language.Parser (ledger)
import Numis.Transform.Denormalize (Sign (..))
import Numis.Transform.Table (Table (..), toTable)
import Options.Applicative
import System.Exit (die)
import System.IO (IOMode (..), stdout, withFile)
import Text.Megaparsec (parseMaybe)
import Text.Pandoc (
  Alignment (..),
  ColWidth (..),
  Extension (..),
  ReaderOptions (..),
  WriterOptions (..),
  def,
  emptyExtensions,
  enableExtension,
  githubMarkdownExtensions,
  runIOorExplode,
  writeMarkdown,
 )
import Text.Pandoc.Builder (
  Blocks,
  Caption (..),
  Cell (..),
  ColSpan (..),
  ColSpec,
  Row (..),
  RowHeadColumns (..),
  RowSpan (..),
  TableBody (..),
  TableFoot (..),
  TableHead (..),
  doc,
  emptyCell,
  nullAttr,
  plain,
  simpleCell,
  simpleTable,
  table,
  text,
  toList,
 )
import Text.Pandoc.Readers
import Text.Pandoc.Templates (compileDefaultTemplate, getDefaultTemplate)
import Text.Pandoc.Writers (writeCommonMark, writeHtml5String, writeLaTeX, writeNative, writeRST)
import Text.Pretty.Simple (pPrint)
import Prelude hiding (writeFile)

parsePrintCommand :: Parser Command
parsePrintCommand = CommandPrint <$> sourceParser <*> outputParser <*> formatParser

runPrint :: FilePath -> Maybe FilePath -> OutputFormat -> IO ()
runPrint fp mo fmt = withFile fp ReadMode \h -> do
  contents <- TIO.hGetContents h
  l <- handleNothing (parseMaybe ledger contents) $ setErrorColor >> die "Failed to parse source"
  out <- runIOorExplode $ do
    template <- case fmt of 
      HTML -> compileDefaultTemplate "html"
      MD -> compileDefaultTemplate "markdown"
      LaTeX -> compileDefaultTemplate "latex"
    let writer =
          case fmt of
            HTML ->
              writeHtml5String
                ( def
                    { writerExtensions =
                        Ext_grid_tables `enableExtension` emptyExtensions
                    , writerTemplate = Just template
                    }
                )
            MD ->
              writeMarkdown
                ( def
                    { writerExtensions =
                        Ext_grid_tables `enableExtension` emptyExtensions
                    , writerTemplate = Just template
                    }
                )
            LaTeX ->
              writeLaTeX
                ( def
                    { writerExtensions =
                        Ext_grid_tables `enableExtension` emptyExtensions
                    , writerTemplate = Just template
                    }
                )
    writer . doc . toPandocTable . toTable $ l
  case mo of
    Nothing -> TIO.hPutStr stdout out
    Just ofp -> TIO.writeFile ofp out

toPandocTable :: Table -> Blocks
toPandocTable (Table{..}) = table caption colSpec tableHead tableBody tableFoot
 where
  descs :: Maybe (V.Vector (Maybe Text))
  descs =
    if V.all isNothing descriptions
      then Nothing
      else Just descriptions

  caption :: Caption
  caption = Caption Nothing []

  tableFoot :: TableFoot
  tableFoot = TableFoot nullAttr []

  -- colSpec :: [ColSpec]
  -- colSpec =
  --   let numCols = (length headers * 2) + fromMaybe 0 (1 <$ descs)
  --    in replicate numCols (AlignDefault, ColWidth (1 / realToFrac numCols))

  colSpec :: [ColSpec]
  colSpec =
    let numCols = (length headers * 2) + fromMaybe 0 (1 <$ descs)
     in replicate numCols (AlignDefault, ColWidthDefault)

  topHeader :: Row
  topHeader =
    Row nullAttr
      $ ( V.toList headers <&> \hText ->
            Cell
              nullAttr
              AlignDefault
              (RowSpan 1)
              (ColSpan 2)
              (toList $ plain $ text hText)
        )
      ++ fromMaybe
        []
        ( descs
            $> [ Cell
                  nullAttr
                  AlignDefault
                  (RowSpan 2)
                  (ColSpan 1)
                  (toList $ plain $ text "Description")
               ]
        )

  bottomHeader :: Row
  bottomHeader =
    Row nullAttr
      $ concat
      . transpose
      $ [ V.toList $ headers $> simpleCell (plain $ text "Asset")
        , V.toList $ headers $> simpleCell (plain $ text "Liability")
        ]

  tableHead :: TableHead
  tableHead = TableHead nullAttr [topHeader, bottomHeader]

  tableBody :: [TableBody]
  tableBody = singleton
    $ TableBody nullAttr (RowHeadColumns 0) []
    $ V.toList
    $ flip
      V.imap
      rows
      \i row ->
        Row nullAttr
          $ ( V.toList row <&> \case
                Nothing -> emptyCell
                Just (Positive n) -> simpleCell . plain . text $ "+" <> (T.pack $ show n)
                Just (Negative n) -> simpleCell . plain . text $ "-" <> (T.pack $ show n)
            )
          ++ fromMaybe
            []
            ( do
                d <- descs
                t <- d V.!? i
                singleton . simpleCell . plain . text <$> t
            )
