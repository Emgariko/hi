module HW3.Pretty (
    prettyValue
    ) where

import HW3.Base (HiValue (..))
import Prettyprinter ( Doc, Pretty (pretty), brackets, space, (<+>), viaShow )
import Prettyprinter.Render.Terminal (AnsiStyle)
import GHC.Real (Ratio((:%)))
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Char (toLower, isUpper)
import Text.Casing (fromHumps, toKebab)
import Data.List (intercalate)
import Data.Foldable (Foldable(toList))
import Data.ByteString (unpack)
import Text.Printf (printf)

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
                Just _ -> pretty (
                        (if quot == 0 then "" else show quot ++ sign) ++
                        ((if quot == 0 && rem < 0 then "-" else "" ) ++ show (abs rem)) ++ "/" ++ show b
                    )
                Nothing -> pretty . show $ sc

prettyValue (HiValueBool val) = pretty (if val then "true" else "false")
prettyValue (HiValueFunction val) = pretty $ toKebab . fromHumps $ drop 5 $ show val
prettyValue (HiValueString val) = pretty (show val)
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList xs) = brackets $ space <> pretty (intercalate ", " $ map (show . prettyValue) $ toList xs) <> space
prettyValue (HiValueBytes bts) = brackets (pretty "#" <+> pretty (unwords $ map (printf "%02x") $ unpack bts) <+> pretty "#")
prettyValue (HiValueAction act) = viaShow act --TODO: do norm prerttValue