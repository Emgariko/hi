{-# LANGUAGE DeriveGeneric #-}

module HW3.Base (
    HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..),
    HiMonad (..),
    ) where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import qualified GHC.Generics as Generics
import Codec.Serialise (Serialise)

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
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  deriving (Show, Eq, Ord, Generics.Generic)

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  deriving (Show, Eq, Ord, Generics.Generic)

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

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd 
  deriving Generics.Generic

instance Serialise HiFun
instance Serialise HiValue
instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue