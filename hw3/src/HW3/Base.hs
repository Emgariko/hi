module HW3.Base (
    HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..)
    ) where

import Data.Text (Text)
import Data.Sequence (Seq)

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
  | HiFunIf 
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim 
  | HiFunList
  | HiFunRange
  | HiFunFold
  deriving (Show, Eq, Ord)

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
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
