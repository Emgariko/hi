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
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf deriving (Show, Eq, Ord)

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  deriving (Show, Eq, Ord)

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
