module HW3.Parser (
    parse, pHiFun, pHiValue, pHiExpr
    ) where

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..), HiAction (..))
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, (<|>), parseTest, choice, many, runParser, MonadParsec (eof, notFollowedBy, try), between, manyTill, optional)
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Data.Scientific
import Text.Megaparsec.Char (char, space, string, hexDigitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack)
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixL, InfixR, InfixN))
import Data.ByteString (ByteString, pack)
import Data.Char (digitToInt)
import Data.Word (Word8)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)

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
  , HiFunTrim <$ string "trim"
  , HiFunList <$ symbol "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  , HiFunPackBytes <$ string "pack-bytes"
  , HiFunUnpackBytes <$ string "unpack-bytes"
  , HiFunZip <$ string "zip"
  , HiFunUnzip <$ string "unzip"
  , HiFunEncodeUtf8 <$ string "encode-utf8"
  , HiFunDecodeUtf8 <$ string "decode-utf8"
  , HiFunSerialise <$ string "serialise"
  , HiFunDeserialise <$ string "deserialise"
  , HiFunRead <$ string "read"
  , HiFunWrite <$ string "write"
  , HiFunMkDir <$ string "mkdir"
  , HiFunChDir <$ string "cd"
  , HiFunParseTime <$ string "parse-time"
  , HiFunRand <$ string "rand"
  , HiFunEcho <$ string "echo"]

pHiValueNumber :: Parser Data.Scientific.Scientific
pHiValueNumber = L.signed (return ()) $ lexeme scientific

pHiValueBool :: Parser Bool
pHiValueBool = lexeme $ choice
  [ True <$ string "true"
  , False <$ string "false"]

charLiteral :: Parser Char
charLiteral = between (char '\"') (char '\"') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = lexeme $ Data.Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pHiValueByte :: Parser Word8
pHiValueByte = lexeme $ do
  a0 <- hexDigitChar
  a1 <- hexDigitChar
  let val = fromIntegral $ digitToInt a0 * 16 + digitToInt a1
  return val

-- TODO: check can [ #     #    ] be parsed ?
pHiValueBytes :: Parser ByteString
pHiValueBytes = do
  _ <- symbol "[#"
  bytes <- many pHiValueByte
  _ <- symbol "#]"
  return $ Data.ByteString.pack bytes

pHiValue :: Parser HiValue
pHiValue = choice
  [ HiValueFunction <$> pHiFun
  , HiValueNumber . toRational <$> pHiValueNumber
  , HiValueBool <$> pHiValueBool
  , HiValueNull <$ symbol "null"
  , HiValueString <$> stringLiteral
  , HiValueBytes <$> pHiValueBytes
  , HiValueAction HiActionCwd <$ symbol "cwd"
  , HiValueAction HiActionNow <$ symbol "now"]

pHiExprArgs :: Parser [HiExpr]
pHiExprArgs = do
  arg <- pHiExprOps
  rest <- many (lexeme (char ',') *> pHiExprOps)
  return (arg : rest)
-- Args   -> HiExpr ArgEnd
-- ArgEnd -> , HiExpr ArgEnd
-- ArgEnd -> eps

pHiExprParenthesesArgs :: HiExpr -> Parser HiExpr
pHiExprParenthesesArgs expr = do
  _ <- lexeme $ char '('
  exprArgs <- pHiExprArgs <|> pure [] -- FIXME: ?
  _ <- lexeme $ char ')'
  return $ HiExprApply expr exprArgs

pHiExprRun :: HiExpr -> Parser HiExpr
pHiExprRun expr = do
  _ <- symbol "!"
  return $ HiExprRun expr

pHiExpr_ :: HiExpr -> Parser HiExpr
pHiExpr_ expr = do
  expr_ <- pHiExprRun expr <|> pHiExprParenthesesArgs expr
  pHiExpr_ expr_ <|> pure expr_

pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> pHiValue

pBracketsList :: Parser HiExpr
pBracketsList = do
  _ <- symbol "["
  args <- optional pHiExprArgs
  _ <- symbol "]"
  let args_ = fromMaybe [] args
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) args_

pHiExpr :: Parser HiExpr
pHiExpr =
  -- (do
  -- _ <- lexeme $ char '('
  -- e <- pHiExpr
  -- _ <- lexeme $ char ')'
  -- return e
  -- ) <|>
    do
  v <- pHiExprValue <|> parens pHiExprOps <|> pBracketsList
  pHiExpr_ v <|> pure v

-- HiExpr   -> hv HiExpr'
-- HiExpr'  -> (Args) HiExpr'
-- HiExpr'  -> eps

-- pTerm :: Parser HiExpr
-- pTerm = choice
--   [ pHiExpr
--   ]

pHiExprOps :: Parser HiExpr
pHiExprOps = makeExprParser pHiExpr operatorTable

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

getHiFun :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
getHiFun fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryL name f = InfixL (f <$ symbol name)

binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryR name f = InfixR (f <$ symbol name)

binary :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binary name f = InfixN (f <$ symbol name)