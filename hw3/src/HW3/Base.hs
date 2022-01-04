module HW3.Base (
    HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..)
    ) where

-- FIXME: deriving Show

-- function names (e.g. div, sort, length, ...)
data HiFun = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  deriving Show

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueNumber Rational
  | HiValueFunction HiFun
  deriving Show

-- expressions (literals, function calls, ...)
data HiExpr = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving Show

-- evaluation errors (invalid arguments, ...)
data HiError = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show
