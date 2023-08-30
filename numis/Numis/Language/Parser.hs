module Numis.Language.Parser where

import Prelude hiding (lex)
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, digitChar, alphaNumChar, spaceChar)
import Control.Monad (void)
import Data.Foldable
import qualified Data.Text as T
import Numeric.Natural
import Numis.Payment
import Data.Functor
import Numis.Language.Expr

-- General parsers
sp :: Parsec Void Text ()
sp = L.space space1 lineComment empty

sym :: Text -> Parsec Void Text Text
sym = L.symbol sp

lex :: Parsec Void Text a -> Parsec Void Text a
lex = L.lexeme sp

lineComment :: Parsec Void Text ()
lineComment = void $ L.skipLineComment "--"

identifier :: Parsec Void Text Text
identifier = T.pack <$> lex (some alphaNumChar)

amount :: Parsec Void Text Natural
amount = read <$> lex (some digitChar)

text :: Parsec Void Text Text
text = T.pack <$> lex (some (try alphaNumChar <|> spaceChar))

to :: Parsec Void Text ()
to = void $ sym "to" 

as :: Parsec Void Text ()
as = void $ sym "as" 

quote :: Parsec Void Text ()
quote = void $ sym "\"" 

-- Operator parsers
has :: Parsec Void Text (a -> Scalar a)
has = sym "has" $> Asset

owes :: Parsec Void Text (a -> Scalar a) 
owes = sym "owes" $> Liability

assigns :: Parsec Void Text (a -> Settlement a)
assigns = sym "assigns" $> Assignment

issues :: Parsec Void Text (a -> Settlement a)
issues = sym "issues" $> Issuance

novates :: Parsec Void Text (a -> Settlement a)
novates = sym "novates" $> Novation

setsOff :: Parsec Void Text (a -> Settlement a)
setsOff = sym "sets-off" $> SetOff

-- Complex parsers
scalar :: Parsec Void Text (Scalar Natural)
scalar = do
  op <- try has <|> owes
  op <$> amount

settlementP :: Parsec Void Text (Settlement Natural)
settlementP = do
  op <- asum [assigns, try issues, try novates, try setsOff]
  op <$> amount

statementP :: Parsec Void Text (Statement Natural)
statementP = do 
  statementSettlement <- settlementP
  statementTitle <- optional $ as >> between quote quote identifier
  pure $ Statement{..} 

fact :: Parsec Void Text Fact
fact = liftA2 (,) identifier scalar

payment :: Parsec Void Text Payment'
payment = do
  payer <- identifier
  statementSettlement <- settlementP
  to
  payee <- identifier
  statementTitle <- optional $ as >> between quote quote text
  let settlement = Statement{..}
  pure $ Payment{..}

factRel :: Parsec Void Text (Either Fact Payment')
factRel = Left <$> try fact 
      <|> (Right <$> payment)

ledger :: Parsec Void Text Ledger
ledger = do
  lex $ many lineComment
  lex $ many factRel
