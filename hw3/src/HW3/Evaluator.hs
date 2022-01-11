{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator (
    eval
    ) where

import HW3.Base (HiExpr (..), HiError (..), HiValue (..), HiFun (..), HiMonad (runAction), HiAction (..))
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, runExcept, foldM, MonadTrans (lift))
import HW3.Parser (parse)
import Data.Text (toUpper, toLower, strip, intercalate, pack, Text, index, singleton, append, length, take, drop, reverse, dropEnd, unpack)
import GHC.Base (stimes)
import GHC.Real (Ratio((:%)))
import Data.Sequence (fromList, ViewL ((:<), EmptyL), viewl, length, reverse, (><), take, Seq, drop, singleton, index, splitAt)
import qualified Data.ByteString
import Data.Foldable (Foldable(toList))
import Data.Word (Word8)
import Data.ByteString (unpack, ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Codec.Compression.Zlib (CompressParams(compressLevel), compressWith, bestCompression, defaultCompressParams, decompressWith, defaultDecompressParams)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Codec.Serialise (serialise, deserialise)
import Text.Read (readMaybe)
import Data.Time (addUTCTime)
import Data.Time.Clock (diffUTCTime)
-- import Data.Bitraversable (bimapM)
-- import Data.Bifunctor (Bifunctor(bimap))
-- import qualified Data.Map
-- import Data.Map (Map, (!?))
-- import qualified Data.Maybe

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
getArity HiFunLength = 1
getArity HiFunToUpper = 1
getArity HiFunToLower = 1
getArity HiFunReverse = 1
getArity HiFunTrim = 1
getArity HiFunList = -1
getArity HiFunRange = 2
getArity HiFunFold = 2
getArity HiFunPackBytes = 1
getArity HiFunUnpackBytes = 1
getArity HiFunEncodeUtf8 = 1
getArity HiFunDecodeUtf8 = 1
getArity HiFunZip = 1
getArity HiFunUnzip = 1
getArity HiFunSerialise = 1
getArity HiFunDeserialise = 1
getArity HiFunRead = 1
getArity HiFunWrite = 2
getArity HiFunMkDir = 1
getArity HiFunChDir = 1
getArity HiFunParseTime = 1
getArity HiFunRand = 2
getArity HiFunEcho = 1
-- getArity _ = undefined

checkArity :: HiFun -> [HiExpr] -> Bool
checkArity fun args = let arity = getArity fun in
                    case arity of
                        -1 -> True
                        _ -> arity == Prelude.length args

isInteger :: Rational -> Bool
isInteger (a :% b) = a `mod` b == 0

evalHiFun :: HiMonad m => HiFun -> [HiValue] -> ExceptTm m
evalHiFun HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
evalHiFun HiFunAdd [HiValueString a, HiValueString b] = return $ HiValueString $ Data.Text.append a b
evalHiFun HiFunAdd [HiValueList a, HiValueList b] = return $ HiValueList $ a >< b
evalHiFun HiFunAdd [HiValueTime a, HiValueNumber b] = return $ HiValueTime $ addUTCTime (realToFrac b) a
evalHiFun HiFunAdd [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes $ Data.ByteString.append a b
evalHiFun HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a - b)
evalHiFun HiFunSub [HiValueTime a, HiValueTime b] = return $ HiValueNumber $ toRational $ diffUTCTime a b
evalHiFun HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a * b)
evalHiFun HiFunMul [HiValueString a, HiValueNumber b] = if isInteger b && b > 0
                                                        then return $ HiValueString $ stimes (floor b) a
                                                        else throwError HiErrorInvalidArgument
evalHiFun HiFunMul [HiValueList l, HiValueNumber b] =
    if isInteger b && b > 0
    then return $ HiValueList $ stimes (floor b) l
    else throwError HiErrorInvalidArgument
evalHiFun HiFunMul [HiValueBytes a, HiValueNumber b] =
    if isInteger b && b > 0
    then return $ HiValueBytes $ stimes (floor b) a
    else throwError HiErrorInvalidArgument
evalHiFun HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a / b)
evalHiFun HiFunDiv [HiValueString a, HiValueString b] = return $ HiValueString $ intercalate (Data.Text.singleton '/') [a, b]
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
evalHiFun HiFunLength [HiValueList l] = return $ HiValueNumber $ toRational $ Data.Sequence.length l
evalHiFun HiFunLength [HiValueBytes bts] = return $ HiValueNumber $ toRational $ Data.ByteString.length bts
evalHiFun HiFunToUpper [HiValueString s] = return $ HiValueString $ toUpper s
evalHiFun HiFunToLower [HiValueString s] = return $ HiValueString $ toLower s
evalHiFun HiFunReverse [HiValueString s] = return $ HiValueString $ Data.Text.reverse s
evalHiFun HiFunReverse [HiValueList l] = return $ HiValueList $ Data.Sequence.reverse l
evalHiFun HiFunReverse [HiValueBytes bts] = return $ HiValueBytes $ Data.ByteString.reverse bts
evalHiFun HiFunTrim [HiValueString s] = return $ HiValueString $ strip s
evalHiFun HiFunList l = return $ HiValueList $ fromList l
evalHiFun HiFunRange [HiValueNumber l, HiValueNumber r] = return $ HiValueList $ fromList $ map HiValueNumber [l..r]
evalHiFun HiFunFold [HiValueFunction fun, HiValueList l] =
        case viewl l of
            EmptyL -> return HiValueNull
            (x :< xs) -> foldM (\x y -> evalHiFun fun [x, y]) x xs
evalHiFun HiFunPackBytes [HiValueList l] =
    let values = map fromValue $ toList l
        ok = all (\case
                Just _ -> True
                _ -> False)
            values
        in if ok
        then return $ HiValueBytes $ Data.ByteString.pack $
            map (\case
                    Just w -> w
                    _ -> error "unexcpected")
             values
        else throwError HiErrorInvalidArgument
    where
        fromValue :: HiValue -> Maybe Word8
        fromValue (HiValueNumber val@(x :% y)) = if x `mod` y == 0 && val < 256
                                        then Just $ fromIntegral $ div x y
                                        else Nothing
        fromValue _ = Nothing
evalHiFun HiFunUnpackBytes [HiValueBytes bts] = return $ HiValueList $ fromList $ map (HiValueNumber . toRational) $ Data.ByteString.unpack bts
evalHiFun HiFunEncodeUtf8 [HiValueString s] = return $ HiValueBytes $ encodeUtf8 s
evalHiFun HiFunDecodeUtf8 [HiValueBytes bts] = return $ case decodeUtf8' bts of
                                                            Left _ -> HiValueNull
                                                            Right s -> HiValueString s
evalHiFun HiFunZip [HiValueBytes bts] = return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict bts
evalHiFun HiFunUnzip [HiValueBytes bts] = return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict bts
evalHiFun HiFunSerialise [v] = return $ HiValueBytes $ toStrict $ serialise v
evalHiFun HiFunDeserialise [HiValueBytes bts] = return $ deserialise $ fromStrict bts
evalHiFun HiFunRead [HiValueString path] = return $ HiValueAction $ HiActionRead $ Data.Text.unpack path
evalHiFun a@HiFunWrite [v1@(HiValueString path), HiValueString s] = evalHiFun a [v1, HiValueBytes $ encodeUtf8 s]
evalHiFun HiFunWrite [HiValueString path, HiValueBytes bts] = return $ HiValueAction $ HiActionWrite (Data.Text.unpack path) bts
evalHiFun HiFunMkDir [HiValueString path] = return $ HiValueAction $ HiActionMkDir (Data.Text.unpack path)
evalHiFun HiFunChDir [HiValueString path] = return $ HiValueAction $ HiActionChDir (Data.Text.unpack path)
evalHiFun HiFunParseTime [HiValueString s] =
    return $ maybe HiValueNull HiValueTime (readMaybe $ Data.Text.unpack s)
evalHiFun HiFunRand [HiValueNumber a@(a1 :% a2), HiValueNumber b@(b1 :% b2)] =
    let aa = a1
        bb = b1
    in if a <= b && isInteger a && isInteger b && fromIntegral (minBound :: Int) <= aa && aa <= fromIntegral (maxBound :: Int) 
        && fromIntegral (minBound :: Int) <= bb && bb <= fromIntegral (maxBound :: Int) 
    then return $ HiValueAction $ HiActionRand (fromIntegral aa) (fromIntegral bb)
    else throwError HiErrorInvalidArgument
evalHiFun HiFunEcho [HiValueString s] = return $ HiValueAction $ HiActionEcho s
evalHiFun _ _ = throwError HiErrorInvalidArgument

validateArgs :: HiFun -> [HiValue] -> Maybe HiError
validateArgs HiFunDiv [v1, v2] = case v2 of
                                    (HiValueNumber 0) -> Just HiErrorDivideByZero
                                    _ -> Nothing
validateArgs _ _ = Nothing

validateArgsAndThenEval :: HiMonad m => HiFun -> [HiExpr] -> ExceptTm m
validateArgsAndThenEval fun es = do
    argVals <- traverse evalHiExpr es
    let ok = validateArgs fun argVals
    case ok of
        Nothing -> evalHiFun fun argVals
        Just err -> throwError err

checkLazyEval :: HiMonad m => HiFun -> [HiExpr] -> ExceptTm m
checkLazyEval HiFunIf [cond, trueBranch, falseBranch] = do
    condV <- evalHiExpr cond
    case condV of
        HiValueBool True -> evalHiExpr trueBranch
        HiValueBool False -> evalHiExpr falseBranch
        _ -> throwError HiErrorInvalidArgument

checkLazyEval HiFunAnd [left, right] = do
    leftV <- evalHiExpr left
    case leftV of
        left_@(HiValueBool False) -> return left_
        left_@HiValueNull -> return left_
        _ -> evalHiExpr right

checkLazyEval HiFunOr [left, right] = do
    leftV <- evalHiExpr left
    case leftV of
        (HiValueBool False) -> evalHiExpr right
        HiValueNull -> evalHiExpr right
        _ -> return leftV
checkLazyEval fun args = validateArgsAndThenEval fun args

checkArityAndThenEval :: HiMonad m => HiFun -> [HiExpr] -> ExceptTm m
checkArityAndThenEval fun args =
    let ok = checkArity fun args in
        if ok
            then checkLazyEval fun args
            else throwError HiErrorArityMismatch


evalString :: HiMonad m => Text -> [HiValue] -> ExceptTm m
evalString s [HiValueNull, HiValueNull] = return $ HiValueString s
evalString s [HiValueNull, r] = evalString s [HiValueNumber 0, r]
evalString s [l, HiValueNull] = evalString s [l, HiValueNumber $ toRational . Data.Text.length $ s]
evalString s [HiValueNumber ind@(a :% b)] = case a `mod` b of
                                    0 -> let len = Data.Text.length s
                                             ind_ = floor ind in
                                        return $ if ind >= 0 && ind_ < len
                                            then HiValueString $ Data.Text.singleton $ Data.Text.index s ind_
                                            else HiValueNull
                                    _ -> throwError HiErrorInvalidArgument
evalString s [HiValueNumber l@(al :% bl),
            HiValueNumber r@(ar :% br)] = if al `mod` bl == 0 && ar `mod` br == 0
                                            then (let len = Data.Text.length s
                                                      l1 = floor l
                                                      r1 = floor r
                                                      l_ = if l1 < 0 then l1 + len else l1
                                                      r_ = if r1 < 0 then r1 + len else r1
                                                    in return $ HiValueString $ Data.Text.drop l_ (Data.Text.dropEnd (len - r_) s))
                                            else throwError HiErrorInvalidArgument

evalString _ _ = throwError HiErrorInvalidArgument

evalList :: HiMonad m => Seq HiValue -> [HiValue] -> ExceptTm m
evalList s [HiValueNull, HiValueNull] = return $ HiValueList s
evalList s [HiValueNull, r] = evalList s [HiValueNumber 0, r]
evalList s [l, HiValueNull] = evalList s [l, HiValueNumber $ toRational . Data.Sequence.length $ s]
evalList seq [HiValueNumber ind@(a :% b)] = case a `mod` b of
                                    0 -> let len = Data.Sequence.length seq
                                             ind_ = floor ind in
                                        return $ if ind >= 0 && ind_ < len
                                            then Data.Sequence.index seq ind_
                                            else HiValueNull
                                    _ -> throwError HiErrorInvalidArgument

evalList seq [HiValueNumber l@(al :% bl),
            HiValueNumber r@(ar :% br)] = if al `mod` bl == 0 && ar `mod` br == 0
                                            then (let len = Data.Sequence.length seq
                                                      l1 = floor l
                                                      r1 = floor r
                                                      l_ = if l1 < 0 then l1 + len else l1
                                                      r_ = if r1 < 0 then r1 + len else r1
                                                    in return $ HiValueList $ Data.Sequence.drop l_ (let (first, _) = Data.Sequence.splitAt r_ seq in first))
                                            else throwError HiErrorInvalidArgument
evalList _ _ = throwError HiErrorInvalidArgument

evalBytes :: HiMonad m => ByteString -> [HiValue] -> ExceptTm m
evalBytes bts [HiValueNull, HiValueNull] = return $ HiValueBytes bts
evalBytes bts [HiValueNull, r] = evalBytes bts [HiValueNumber 0, r]
evalBytes bts [l, HiValueNull] = evalBytes bts [l, HiValueNumber $ toRational . Data.ByteString.length $ bts]
evalBytes bts [HiValueNumber ind@(a :% b)] = case a `mod` b of
                                    0 -> let len = Data.ByteString.length bts
                                             ind_ = floor ind in
                                        return $ if ind_ >= 0 && ind_ < len
                                            then HiValueNumber $ toRational $ Data.ByteString.index bts ind_
                                            else HiValueNull
                                    _ -> throwError HiErrorInvalidArgument
evalBytes bts [HiValueNumber l@(al :% bl),
            HiValueNumber r@(ar :% br)] = if al `mod` bl == 0 && ar `mod` br == 0
                                            then (let len = Data.ByteString.length bts
                                                      l1 = floor l
                                                      r1 = floor r
                                                      l_ = if l1 < 0 then l1 + len else l1
                                                      r_ = if r1 < 0 then r1 + len else r1
                                                    in return $ HiValueBytes $ Data.ByteString.drop l_ (let (first, _) = Data.ByteString.splitAt r_ bts in first))
                                            else throwError HiErrorInvalidArgument
evalBytes _ _ = throwError HiErrorInvalidArgument

-- evalDict :: HiMonad m => Map HiValue HiValue -> HiValue -> ExceptTm m
-- evalDict dict val =
--     return $ Data.Maybe.fromMaybe HiValueNull (dict !? val)

evalHiExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptTm m
evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = checkArityAndThenEval hiFun args
evalHiExprApply (HiExprValue (HiValueString s)) args = do
    argVals <- traverse evalHiExpr args
    evalString s argVals
evalHiExprApply (HiExprValue (HiValueList seq)) args = do
    argVals <- traverse evalHiExpr args
    evalList seq argVals
evalHiExprApply (HiExprValue (HiValueBytes bts)) args = do
    argVals <- traverse evalHiExpr args
    evalBytes bts argVals
evalHiExprApply (HiExprApply e args) args1 = do
    res <- evalHiExprApply e args
    evalHiExprApply (HiExprValue res) args1
-- evalHiExprApply (HiExprDict dict) args = do
--     dict_ < evalHiExpr dict
--     case args of
--         [x] -> do
--             arg <- evalHiExpr x
--             evalDict dict arg
--         _ -> throwError HiErrorArityMismatch

-- evalHiExprApply (HiExprValue (HiValueDict dict)) args = do
--     -- dict_ <- evalHiExpr dict
--     case args of
--         [x] -> do
--             arg <- evalHiExpr x
--             evalDict dict arg
--         _ -> throwError HiErrorArityMismatch

evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: HiMonad m => HiExpr -> ExceptTm m
evalHiExpr (HiExprValue a) = return a
evalHiExpr (HiExprRun expr) = do
    x <- evalHiExpr expr
    case x of
        (HiValueAction act) -> lift $ runAction act
        _ -> throwError HiErrorInvalidArgument
-- evalHiExpr (HiExprDict pairs) = do
--     left <- mapM (evalHiExpr . fst) pairs
--     right <- mapM (evalHiExpr . snd) pairs
--     return $ HiValueDict $ Data.Map.fromList $ zip left right

evalHiExpr (HiExprApply expr args) = evalHiExprApply expr args

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalHiExpr expr)
