{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HW3.Evaluator (
    eval
    ) where

import HW3.Base (HiExpr (..), HiError (HiErrorInvalidFunction, HiErrorArityMismatch, HiErrorInvalidArgument), HiValue (..), HiFun (HiFunDiv, HiFunAdd, HiFunSub, HiFunMul))
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, MonadError (throwError))

-- type ExceptTm m = ExceptT HiError m HiValue
type ExceptTm = ExceptT HiError Identity HiValue

getArity :: HiFun -> Int
getArity HiFunDiv = 2
getArity HiFunMul = 2
getArity HiFunSub = 2
getArity HiFunAdd = 2

checkArity :: HiFun -> [HiExpr] -> Bool 
checkArity fun args = getArity fun == length args 

evalHiFun :: HiFun -> [HiExpr] -> ExceptTm
evalHiFun HiFunAdd [e1, e2] = do
                            v1 <- evalHiExpr e1
                            v2 <- evalHiExpr e2
                            return (HiValueNumber (v1 + v2))
                            
evalHiFun HiFunDiv [e1, e2] = undefined 

checkArityAndThenEval :: HiFun -> [HiExpr] -> ExceptTm
checkArityAndThenEval fun args = 
    let ok = checkArity fun args in
        if ok
            then evalHiFun fun args
            else throwError HiErrorArityMismatch
                

evalHiExprApply :: HiExpr -> [HiExpr] -> ExceptTm
evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = checkArityAndThenEval hiFun args
evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: HiExpr -> ExceptTm
evalHiExpr (HiExprValue a@(HiValueNumber _)) = return a
evalHiExpr (HiExprValue (HiValueFunction _)) = throwError HiErrorInvalidArgument
evalHiExpr (HiExprApply expr args) = evalHiExprApply expr args

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
-- eval expr = evalHiExpr expr
eval = undefined 