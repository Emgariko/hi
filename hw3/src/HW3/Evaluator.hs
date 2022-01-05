module HW3.Evaluator (
    eval
    ) where

import HW3.Base (HiExpr (..), HiError (..), HiValue (..), HiFun (HiFunDiv, HiFunAdd, HiFunSub, HiFunMul))
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, runExcept)
import Data.Functor.Classes (eq1)

-- type ExceptTm m = ExceptT HiError m HiValue
type ExceptTm m = ExceptT HiError m HiValue

mapToOperation :: HiFun -> (Rational -> Rational -> Rational)
mapToOperation HiFunAdd = (+)
mapToOperation HiFunSub = (-)
mapToOperation HiFunMul = (*)
mapToOperation HiFunDiv = (/)

getArity :: HiFun -> Int
getArity HiFunDiv = 2
getArity HiFunMul = 2
getArity HiFunSub = 2
getArity HiFunAdd = 2

checkArity :: HiFun -> [HiExpr] -> Bool
checkArity fun args = getArity fun == length args

evalHiFun_ :: HiValue -> HiValue -> (Rational -> Rational -> Rational) -> HiValue
evalHiFun_ (HiValueNumber v1) (HiValueNumber v2) f = HiValueNumber (f v1 v2)
evalHiFun_ _ _ _ = error "unexcepted function call"

validateArgs :: HiFun -> HiValue -> HiValue -> Maybe HiError
validateArgs HiFunDiv (HiValueNumber v1) (HiValueNumber v2) = case v2 of
                                                                0 -> Just HiErrorDivideByZero
                                                                _ -> Nothing
validateArgs _ _ _ = Nothing

twoArgFun :: Monad m => HiFun -> HiExpr -> HiExpr -> (Rational -> Rational -> Rational) -> ExceptTm m
twoArgFun fun e1 e2 f = do
    v1 <- evalHiExpr e1
    v2 <- evalHiExpr e2
    let ok = validateArgs fun v1 v2
    case ok of
        Nothing -> return $ evalHiFun_ v1 v2 f
        Just err -> throwError err

evalHiFun :: Monad m => HiFun -> [HiExpr] -> ExceptTm m
evalHiFun fun [e1, e2] = twoArgFun fun e1 e2 $ mapToOperation fun

evalHiFun _ _ = error "unexpected function call"

checkArityAndThenEval :: Monad m => HiFun -> [HiExpr] -> ExceptTm m
checkArityAndThenEval fun args =
    let ok = checkArity fun args in
        if ok
            then evalHiFun fun args
            else throwError HiErrorArityMismatch


evalHiExprApply :: Monad m => HiExpr -> [HiExpr] -> ExceptTm m
evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = checkArityAndThenEval hiFun args
evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: Monad m => HiExpr -> ExceptTm m
evalHiExpr (HiExprValue a@(HiValueNumber _)) = return a
evalHiExpr (HiExprValue (HiValueFunction _)) = throwError HiErrorInvalidArgument
evalHiExpr (HiExprApply expr args) = evalHiExprApply expr args

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
-- eval expr = evalHiExpr expr
eval expr = runExceptT (evalHiExpr expr)