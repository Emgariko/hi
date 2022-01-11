{-# LANGUAGE DeriveGeneric #-}

module HW3.Base (
    HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..),
    HiMonad (..),
    HiAction (..)
    ) where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import qualified GHC.Generics as Generics
import Codec.Serialise (Serialise)
import Data.Time (UTCTime)
import Data.Map (Map)

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
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  -- | HiFunCount
  -- | HiFunKeys
  -- | HiFunValues
  -- | HiFunInvert
  deriving (Show, Eq, Ord, Generics.Generic)

-- | HiFunRead
  -- | HiFunWrite
  -- | HiFunMkDir
  -- | HiFunChDir

-- values (numbers, booleans, strings, ...)
data HiValue = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  -- | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generics.Generic)

-- expressions (literals, function calls, ...)
data HiExpr = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  -- | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Show)

-- evaluation errors (invalid arguments, ...)
data HiError = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Show)

data HiAction = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd 
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generics.Generic)

instance Serialise HiFun
instance Serialise HiValue
instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue