module HW3.Parser (
    parse
    ) where

import HW3.Base (HiExpr)
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = undefined