module HW3.Parser (
    parse, pHiFun, pHiValue, pHiExpr
    ) where

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..), HiAction (..))
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle )
import Text.Megaparsec (Parsec, (<|>), parseTest, choice, many, runParser, MonadParsec (eof, notFollowedBy, try), between, manyTill, optional, sepBy1, satisfy)
import Text.Megaparsec.Char.Lexer (scientific)
import qualified Data.Scientific
import Text.Megaparsec.Char (char, space, string, hexDigitChar, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack)
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixL, InfixR, InfixN))
import Data.ByteString (ByteString, pack)
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Word (Word8)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
-- import Data.List (intercalate)

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
  , HiFunEcho <$ string "echo"
  -- , HiFunCount <$ string "count"
  -- , HiFunKeys <$ string "keys"
  -- , HiFunValues <$ string "values"
  -- , HiFunInvert <$ string "invert"
  ]

pHiValueNumber :: Parser Data.Scientific.Scientific
pHiValueNumber = L.signed space $ lexeme scientific

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
  a1 <- hexDigitChar <* notFollowedBy digitChar
  let val = fromIntegral $ digitToInt a0 * 16 + digitToInt a1
  return val

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
  exprArgs <- pHiExprArgs <|> pure []
  _ <- lexeme $ char ')'
  return $ HiExprApply expr exprArgs

pHiExprRun :: HiExpr -> Parser HiExpr
pHiExprRun expr = do
  _ <- symbol "!"
  return $ HiExprRun expr

-- pHiExprDotAccess :: HiExpr -> Parser HiExpr
-- pHiExprDotAccess expr = do
--   _ <- symbol "."
--   separated <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
--   let str = intercalate "-" separated
--   return $ HiExprApply expr [HiExprValue $ (HiValueString . Data.Text.pack) str]

pHiExpr_ :: HiExpr -> Parser HiExpr
pHiExpr_ expr = do
  expr_ <- pHiExprRun expr <|> pHiExprParenthesesArgs expr 
  -- <|> pHiExprDotAccess expr
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

-- pDictKeyValue :: Parser (HiExpr, HiExpr)
-- pDictKeyValue = do
--   key <- pHiExpr
--   _ <- symbol ":"
--   value <- pHiExpr
--   return (key, value)

-- -- pDictContent :: Parser [(HiExpr, HiExpr)]
-- -- pDictContent = do
-- --   pair

-- pDict :: Parser HiExpr
-- pDict = do
--   _ <- symbol "{"
--   pair <- optional pDictKeyValue
--   let content = case pair of
--                 Just keyVal -> do
--                   rest <- many (lexeme (char ',') *> pDictKeyValue)
--                   return $ HiExprDict (keyVal : rest)
--                 Nothing -> return $ HiExprDict []
--   res <- content             
--   -- content <- many (lexeme (char ',') *> pDictKeyValue)
--   _ <- symbol "}"
--   return res

pHiExpr :: Parser HiExpr
pHiExpr = do
  v <- pHiExprValue <|> parens pHiExprOps <|> pBracketsList 
  -- <|> pDict
  pHiExpr_ v <|> pure v

pHiExprOps :: Parser HiExpr
pHiExprOps = makeExprParser pHiExpr operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ InfixL  (getHiFun HiFunDiv <$ pDiv)
    , binaryL "*" $ getHiFun HiFunMul
    ]
  , [ binaryL "+" $ getHiFun HiFunAdd
    , binaryL "-" $ getHiFun HiFunSub
    ]
  , [ binary "<=" $ getHiFun HiFunNotGreaterThan
    , binary "<" $ getHiFun HiFunLessThan
    , binary ">=" $ getHiFun HiFunNotLessThan
    , binary ">" $ getHiFun HiFunGreaterThan
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