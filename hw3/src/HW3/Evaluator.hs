module HW3.Evaluator (
    eval, parseKek
    ) where

import HW3.Base (HiExpr (..), HiError (..), HiValue (..), HiFun (..))
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, runExcept)
import HW3.Parser (parse)
import Data.Text (toUpper, toLower, strip, intercalate, pack, Text, index, singleton, append, length, take, drop, reverse)
import GHC.Base (stimes)
import GHC.Real (Ratio((:%)))

-- type ExceptTm m = ExceptT HiError m HiValue
type ExceptTm m = ExceptT HiError m HiValue
-- TODO: rename it

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
getArity HiFunLength = 1
getArity HiFunToUpper = 1
getArity HiFunToLower = 1
getArity HiFunReverse = 1
getArity HiFunTrim = 1

checkArity :: HiFun -> [HiExpr] -> Bool
checkArity fun args = getArity fun == Prelude.length args

evalHiFun :: Monad m => HiFun -> [HiValue] -> ExceptTm m
evalHiFun HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
evalHiFun HiFunAdd [HiValueString a, HiValueString b] = return $ HiValueString $ Data.Text.append a b
evalHiFun HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a - b)
evalHiFun HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a * b)
evalHiFun HiFunMul [HiValueString a, HiValueNumber b] = return $ HiValueString $ stimes (floor b) a    -- TODO: number * str ? 
evalHiFun HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a / b)
evalHiFun HiFunDiv [HiValueString a, HiValueString b] = return $ HiValueString $ intercalate (singleton '/') [a, b]
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
evalHiFun HiFunLength [HiValueString s] = return $ HiValueNumber $ toRational (Data.Text.length s)
evalHiFun HiFunToUpper [HiValueString s] = return $ HiValueString $ toUpper s
evalHiFun HiFunToLower [HiValueString s] = return $ HiValueString $ toLower s
evalHiFun HiFunReverse [HiValueString s] = return $ HiValueString $ Data.Text.reverse s
evalHiFun HiFunTrim [HiValueString s] = return $ HiValueString $ strip s
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

evalString :: Monad m => Text -> [HiValue] -> ExceptTm m
evalString s [HiValueNumber ind@(a :% b)] = case a `mod` b of
                                    0 -> let len = Data.Text.length s
                                             ind_ = floor ind in
                                        return $ if (ind >= 0) && (ind_ < len)
                                            then HiValueString $ singleton $ index s ind_
                                            else HiValueNull
                                    _ -> throwError HiErrorInvalidArgument
evalString s [HiValueNumber l@(al :% bl),
            HiValueNumber r@(ar :% br)] = if (al `mod` bl == 0) && (ar `mod` br == 0) 
                                            then (let len = Data.Text.length s
                                                      l_ = floor l
                                                      r_ = floor r 
                                                    in return $ HiValueString $ Data.Text.take (r_ - l_) (Data.Text.drop l_ s))
                                            else throwError HiErrorInvalidArgument

evalString _ _ = throwError HiErrorInvalidArgument

evalHiExprApply :: Monad m => HiExpr -> [HiExpr] -> ExceptTm m
evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = checkArityAndThenEval hiFun args
evalHiExprApply (HiExprValue (HiValueString s)) args = do
    argVals <- traverse evalHiExpr args
    evalString s argVals

evalHiExprApply (HiExprApply e args) args1 = do
    res <- evalHiExprApply e args
    evalHiExprApply (HiExprValue res) args1
evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: Monad m => HiExpr -> ExceptTm m
evalHiExpr (HiExprValue a@(HiValueNumber _)) = return a
evalHiExpr (HiExprValue a@(HiValueBool _)) = return a
evalHiExpr (HiExprValue a@(HiValueFunction _)) = return a
evalHiExpr (HiExprValue a@(HiValueString _)) = return a
evalHiExpr (HiExprValue a@HiValueNull) = return a
evalHiExpr (HiExprApply expr args) = evalHiExprApply expr args

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
-- eval expr = evalHiExpr expr
eval expr = runExceptT (evalHiExpr expr)

parseKek :: String -> HiExpr
parseKek str = case parse str of
                    (Right x) -> x
                    _ -> undefined

-- TODO: check does eval (if(true, add, mul)(1, 2)) work well.