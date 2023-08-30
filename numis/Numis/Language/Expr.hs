module Numis.Language.Expr where

import Numis.Payment
import Numeric.Natural
import Data.Text (Text)

data Statement a = 
  Statement
    { statementSettlement :: Settlement a
    , statementTitle :: Maybe Text
    } deriving (Eq, Ord, Show, Functor)

type Payment' = Payment Text (Statement Natural)

type Fact = (Text, Scalar Natural)

type Ledger = [Either Fact Payment']
