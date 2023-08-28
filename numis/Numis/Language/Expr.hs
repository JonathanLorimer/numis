module Numis.Language.Expr where

import Numis.Payment
import Numeric.Natural
import Data.Text (Text)

type Payment' = Payment Text (Settlement Natural)
type Fact = (Text, Scalar Natural)
type Ledger = [Either Fact Payment']
