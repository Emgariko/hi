module HW3.Parser (
    parse
    ) where

import HW3.Base (HiExpr, HiError)
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, satisfy, runParser, (<|>))
import Text.Megaparsec.Byte (string)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = undefined

type Parser = Parsec HiError String

pScheme :: Parser String
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"
