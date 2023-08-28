module Numis.Transform.Entities where

import Numis.Language.Expr (Ledger)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Numis.Payment (Payment(..))

agents :: Ledger -> Set Text
agents = foldMap $ either (Set.singleton . fst) \Payment{..} -> Set.fromList [payer, payee]
