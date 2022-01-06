module HW3.Parser (
    parse, pHiFun, pHiValue, pHiExpr
    ) where

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, (<|>), parseTest, choice, many, runParser, MonadParsec (eof, notFollowedBy, try), between, manyTill)
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Data.Scientific
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack)
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixL, InfixR, InfixN))

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pHiExprOps <* eof) ""

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

pHiFun :: Parser HiFun
pHiFun = lexeme $ choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunNot <$ string "not"
  , HiFunAnd <$ string "and"
  , HiFunOr  <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunIf <$ string "if"
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"]

pHiValueNumber :: Parser Data.Scientific.Scientific
pHiValueNumber = L.signed (return ()) $ lexeme scientific

pHiValueBool :: Parser Bool
pHiValueBool = lexeme $ choice
  [ True <$ string "true"
  , False <$ string "false"]

charLiteral :: Parser Char
charLiteral = between (char '\"') (char '\"') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = lexeme $ pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pHiValue :: Parser HiValue
pHiValue = choice
  [ HiValueFunction <$> pHiFun
  , HiValueNumber . toRational <$> pHiValueNumber
  , HiValueBool <$> pHiValueBool
  , HiValueNull <$ symbol "null"
  , HiValueString <$> stringLiteral ]

pHiExprArgs :: Parser [HiExpr]
pHiExprArgs = do
  arg <- pHiExprOps
  rest <- many (lexeme (char ',') *> pHiExprOps)
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
pHiExpr =
  -- (do
  -- _ <- lexeme $ char '('
  -- e <- pHiExpr
  -- _ <- lexeme $ char ')'
  -- return e
  -- ) <|>
    do
  v <- pHiExprValue <|> parens pHiExprOps
  pHiExpr_ v <|> pure v

-- HiExpr   -> hv HiExpr'
-- HiExpr'  -> (Args) HiExpr'
-- HiExpr'  -> eps

pTerm :: Parser HiExpr
pTerm = choice
  [ parens pHiExprOps
  , pHiExpr
  ]

pHiExprOps :: Parser HiExpr
pHiExprOps = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ InfixL  (getHiFun HiFunDiv <$ pDiv)
    -- binaryL "/" $ getHiFun HiFunDiv
    , binaryL "*" $ getHiFun HiFunMul
    ]
  , [ binaryL "+" $ getHiFun HiFunAdd
    , binaryL "-" $ getHiFun HiFunSub
    ]
  , [ binary "<" $ getHiFun HiFunLessThan
    , binary ">" $ getHiFun HiFunGreaterThan
    , binary ">=" $ getHiFun HiFunNotLessThan
    , binary "<=" $ getHiFun HiFunNotGreaterThan
    , binary "==" $ getHiFun HiFunEquals
    , binary "/=" $ getHiFun HiFunNotEquals
    ]
  , [ binaryR "&&" $ getHiFun HiFunAnd ]
  , [ binaryR "||" $ getHiFun HiFunOr ]
  ]
  where
    pDiv :: Parser ()
    pDiv = lexeme . try $ do
      div <- symbol "/"
      _ <- notFollowedBy (char '=')
      return ()

-- TODO: check does (if(true, add, sub))(10, 10) work well.
-- FIXME: (add(1, 2))(3, 4)

getHiFun :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
getHiFun fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryL name f = InfixL (f <$ symbol name)

binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryR name f = InfixR (f <$ symbol name)

binary :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binary name f = InfixN (f <$ symbol name)
