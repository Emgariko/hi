module HW3.Parser (
    parse, pHiFun, pHiValue, pHiExpr
    ) where

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, (<|>), parseTest, choice, many, runParser)
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Data.Scientific
import Text.Megaparsec.Char (char, space, string)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pHiExpr ""

type Parser = Parsec Void String

pHiFun :: Parser HiFun
pHiFun = choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"]

pHiValueNumber :: Parser Data.Scientific.Scientific
pHiValueNumber = scientific

pHiValue :: Parser HiValue
pHiValue = choice
  [ HiValueFunction <$> pHiFun,
    HiValueNumber . toRational <$> pHiValueNumber]

pHiExprArgs :: Parser [HiExpr]
pHiExprArgs = do
  arg <- pHiExpr
  rest <- many (char ',' *> space *> pHiExpr)   -- FIXME: where space should be consumed?
  return (arg : rest)

-- Args   -> HiExpr ArgEnd
-- ArgEnd -> , HiExpr ArgEnd
-- ArgEnd -> eps

pHiExpr_ :: HiExpr -> Parser HiExpr
pHiExpr_ expr = do
  _ <- char '('
  exprArgs <- pHiExprArgs <|> pure [] -- FIXME: ?
  _ <- char ')'
  let expr_ = HiExprApply expr exprArgs
  pHiExpr_  expr_ <|> pure expr_

pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> pHiValue

pHiExpr :: Parser HiExpr
pHiExpr = do
  v <- pHiExprValue
  pHiExpr_ v <|> pure v -- FIXME: ?

-- HiExpr   -> hv HiExpr'
-- HiExpr'  -> (Args) HiExpr'
-- HiExpr'  -> eps
