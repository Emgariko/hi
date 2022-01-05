module HW3.Parser (
    parse, pHiFun, pHiValue, pHiExpr
    ) where

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, (<|>), parseTest, choice, many, runParser, MonadParsec (eof))
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Data.Scientific
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pHiExpr <* eof) ""

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

pHiFun :: Parser HiFun
pHiFun = lexeme $ choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"]

pHiValueNumber :: Parser Data.Scientific.Scientific
pHiValueNumber = lexeme scientific

pHiValue :: Parser HiValue
pHiValue = choice
  [ HiValueFunction <$> pHiFun,
    HiValueNumber . toRational <$> pHiValueNumber]

pHiExprArgs :: Parser [HiExpr]
pHiExprArgs = do
  arg <- pHiExpr
  rest <- many (lexeme (char ',') *> pHiExpr)   -- FIXME: where space should be consumed?
  return (arg : rest)
-- Args   -> HiExpr ArgEnd
-- ArgEnd -> , HiExpr ArgEnd
-- ArgEnd -> eps

pHiExpr_ :: HiExpr -> Parser HiExpr
pHiExpr_ expr = do
  _ <- lexeme $ char '('
  exprArgs <- pHiExprArgs <|> pure [] -- FIXME: ?
  _ <- lexeme $ char ')'
  let expr_ = HiExprApply expr exprArgs
  pHiExpr_  expr_ <|> pure expr_

pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> pHiValue

pHiExpr :: Parser HiExpr
pHiExpr = (do
  _ <- lexeme $ char '('
  e <- pHiExpr
  _ <- lexeme $ char ')'
  return e  
  ) <|> 
    (do
  v <- pHiExprValue 
  pHiExpr_ v <|> pure v)

-- FIXME: (add(1, 2))(3, 4)

-- HiExpr   -> hv HiExpr'
-- HiExpr'  -> (Args) HiExpr'
-- HiExpr'  -> eps
