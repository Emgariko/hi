module HW3.Base (
    HiFun,
    HiValue,
    HiExpr,
    HiError
    ) where

data HiFun = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub    -- function names (e.g. div, sort, length, ...)

data HiValue = HiValueNumber Rational
  | HiValueFunction HiFun -- values (numbers, booleans, strings, ...)

data HiExpr = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr] -- expressions (literals, function calls, ...)
data HiError = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
    deriving Show -- evaluation errors (invalid arguments, ...)