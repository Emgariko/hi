module HW3.Evaluator (
    eval
    ) where

import HW3.Base (HiExpr (HiExprValue), HiError, HiValue (HiValueNumber))
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor.Identity (Identity)

-- type ExceptTm m = ExceptT HiError m HiValue
type ExceptTm = ExceptT HiError Identity HiValue


evalHiExpr :: HiExpr -> ExceptTm
evalHiExpr (HiExprValue a@(HiValueNumber r)) = return a

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = evalHiExpr expr