module HW3.Evaluator (
    eval, parseKek
    ) where

import HW3.Base (HiExpr (..), HiError (..), HiValue (..), HiFun (..))
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, runExcept)
import Data.Functor.Classes (eq1)
import HW3.Parser (parse)

-- type ExceptTm m = ExceptT HiError m HiValue
type ExceptTm m = ExceptT HiError m HiValue

getArity :: HiFun -> Int
getArity HiFunDiv = 2
getArity HiFunMul = 2
getArity HiFunSub = 2
getArity HiFunAdd = 2
getArity HiFunNot = 1
getArity HiFunAnd = 2
getArity HiFunOr = 2
getArity HiFunLessThan = 2
getArity HiFunGreaterThan = 2
getArity HiFunEquals = 2
getArity HiFunNotLessThan = 2
getArity HiFunNotGreaterThan = 2
getArity HiFunNotEquals = 2
getArity HiFunIf = 3

checkArity :: HiFun -> [HiExpr] -> Bool
checkArity fun args = getArity fun == length args

evalHiFun :: Monad m => HiFun -> [HiValue] -> ExceptTm m
evalHiFun HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
evalHiFun HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a - b)
evalHiFun HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a * b)
evalHiFun HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a / b)
evalHiFun HiFunNot [HiValueBool a] = return $ HiValueBool $ not a
evalHiFun HiFunAnd [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a && b
evalHiFun HiFunOr [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a || b
evalHiFun HiFunLessThan [a, b] = return $ HiValueBool $ a < b
evalHiFun HiFunGreaterThan [a, b] = return $ HiValueBool $ a > b 
evalHiFun HiFunEquals [a, b] = return $ HiValueBool $ a == b
evalHiFun HiFunNotLessThan [a, b] = return $ HiValueBool $ a >= b
evalHiFun HiFunNotGreaterThan [a, b] = return $ HiValueBool $ a <= b
evalHiFun HiFunNotEquals [a, b] = return $ HiValueBool $ a /= b
evalHiFun HiFunIf [HiValueBool cond, a, b] = return $ if cond then a else b
evalHiFun _ _ = throwError HiErrorInvalidArgument 

validateArgs :: HiFun -> [HiValue] -> Maybe HiError
validateArgs HiFunDiv [v1, v2] = case v2 of
                                    (HiValueNumber 0) -> Just HiErrorDivideByZero
                                    _ -> Nothing
validateArgs _ _ = Nothing

validateArgsAndThenEval :: Monad m => HiFun -> [HiExpr] -> ExceptTm m
validateArgsAndThenEval fun es = do
    argVals <- traverse evalHiExpr es
    let ok = validateArgs fun argVals
    case ok of
        Nothing -> evalHiFun fun argVals
        Just err -> throwError err

checkArityAndThenEval :: Monad m => HiFun -> [HiExpr] -> ExceptTm m
checkArityAndThenEval fun args =
    let ok = checkArity fun args in
        if ok
            then validateArgsAndThenEval fun args
            else throwError HiErrorArityMismatch

evalHiExprApply :: Monad m => HiExpr -> [HiExpr] -> ExceptTm m
evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = checkArityAndThenEval hiFun args
evalHiExprApply (HiExprApply e args) args1 = do
    res <- evalHiExprApply e args
    evalHiExprApply (HiExprValue res) args1
evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: Monad m => HiExpr -> ExceptTm m
evalHiExpr (HiExprValue a@(HiValueNumber _)) = return a
evalHiExpr (HiExprValue a@(HiValueBool _)) = return a
evalHiExpr (HiExprValue a@(HiValueFunction _)) = return a
evalHiExpr (HiExprApply expr args) = evalHiExprApply expr args

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
-- eval expr = evalHiExpr expr
eval expr = runExceptT (evalHiExpr expr)

parseKek :: String -> HiExpr
parseKek str = case parse str of
                    (Right x) -> x
                    _ -> undefined