module Numis.Language.Expr where

import Numis.Relation
import Numis.Obligation
import Numeric.Natural
import Data.Text (Text)

type Payment' = Payment Text (Settlement Natural)
type Fact = (Text, Scalar Natural)
type Ledger = [Either Fact Payment']
--
-- data Expr i v = 
--     ExprAsset i v 
--   | ExprLiability i v
--   | ExprAssignment i i v 
--   | ExprIssuance i i v 
--   | ExprSetOff i i v 
--   | ExprNovation i i v
