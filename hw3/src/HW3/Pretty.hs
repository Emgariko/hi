module HW3.Pretty (
    prettyValue
    ) where

import Data.ByteString (pack, unpack)
import Data.Char (isUpper, toLower)
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import qualified Data.Map
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Text
import GHC.Real (Ratio ((:%)))
import HW3.Base (HiAction (..), HiFun (..), HiValue (..))
import Prettyprinter (Doc, Pretty (pretty), braces, brackets, comma, enclose, encloseSep, lparen,
                      rparen, space, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Casing (fromHumps, toKebab)
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
                Nothing -> pretty $ formatScientific Fixed Nothing sc


prettyValue (HiValueBool val) = pretty (if val then "true" else "false")
prettyValue (HiValueFunction val) = case val of
                                       HiFunMkDir -> pretty "mkdir"
                                       HiFunChDir -> pretty "cd"
                                       _ -> pretty $ toKebab . fromHumps $ drop 5 $ show val

prettyValue (HiValueString val) = pretty (show val)
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList xs) = brackets $ space <> pretty (intercalate ", " $ map (show . prettyValue) $ toList xs) <> space
prettyValue (HiValueBytes bts) = brackets (pretty "#" <+> pretty (unwords $ map (printf "%02x") $ unpack bts) <+> pretty "#")
prettyValue (HiValueAction act) =
    case act of
        (HiActionRead path) -> prettyAction "read" [prettyValue (HiValueString $ Data.Text.pack path)]
        (HiActionWrite path bts) -> prettyAction "write" [prettyValue (HiValueString $ Data.Text.pack path),  prettyValue (HiValueBytes bts)]
        HiActionMkDir path -> prettyAction "mkdir" [prettyValue (HiValueString $ Data.Text.pack path)]
        HiActionChDir path -> prettyAction "cd" [prettyValue (HiValueString $ Data.Text.pack path)]
        HiActionCwd -> pretty "cwd"
        HiActionNow -> pretty "now"
        HiActionRand l r -> prettyAction "rand" [prettyValue (HiValueNumber . toRational $ l),  prettyValue (HiValueNumber . toRational $ r)]
        HiActionEcho text -> prettyAction "echo" [prettyValue $ HiValueString text]
    where
        prettyAction name args = pretty name <> encloseSep lparen rparen (pretty ", ") args

prettyValue (HiValueTime time) = pretty "parse-time" <> enclose (pretty "(\"") (pretty "\")") (viaShow time)
prettyValue (HiValueDict dict) = braces $ pretty $ intercalate ", " $ map (\(x, y) -> show (prettyValue x <> pretty ":" <+> prettyValue y)) (Data.Map.toList dict)
