module HW3.Pretty (
    prettyValue
    ) where

import HW3.Base (HiValue (..))
import Prettyprinter ( Doc, Pretty (pretty), brackets )
import Prettyprinter.Render.Terminal (AnsiStyle)
import GHC.Real (Ratio((:%)))
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Char (toLower, isUpper)
import Text.Casing (fromHumps, toKebab)
import Data.List (intercalate)
import Data.Foldable (Foldable(toList))

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber val@(a :% b)) =
    let (quot, rem) = quotRem a b in
    if rem == 0
        then pretty (show quot)
        else
            let (sc, s) = fromRationalRepetendUnlimited val
                sign    = if val > 0 then " + " else " - "
            in
            case s of
                Just _ -> pretty (show quot ++ sign ++ show (abs rem) ++ "/" ++ show b)
                Nothing -> pretty . show $ sc

    -- pretty (show val)
-- prettyValue (HiValueNumber val) = pretty (show val)
prettyValue (HiValueBool val) = pretty (if val then "true" else "false")
-- prettyValue (HiValueFunction val) = pretty (map toLower . drop 5 $ show val)
prettyValue (HiValueFunction val) = pretty $ toKebab . fromHumps $ drop 5 $ show val
prettyValue (HiValueString val) = pretty (show val)
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList xs) = brackets $ pretty $ intercalate ", " $ map (show . prettyValue) $ toList xs
-- TODO: add spaces [_ _]  in here (_)