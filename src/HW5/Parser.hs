module HW5.Parser
  ( parse
  ) where

import           Control.Monad.Combinators.Expr
import qualified Data.ByteString                as B
import           Data.Char                      (isAlpha, isAlphaNum, isDigit,
                                                 ord)
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           HW5.Base                       (HiAction (HiActionCwd, HiActionNow),
                                                 HiExpr (HiExprApply, HiExprDict, HiExprRun, HiExprValue),
                                                 HiFun (..),
                                                 HiValue (HiValueAction, HiValueBool, HiValueBytes, HiValueFunction, HiValueNull, HiValueNumber, HiValueString))
import           Text.Megaparsec                hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipWS eof parseExpr) ""

skipWS :: Parser ()
skipWS = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipWS

symbol :: String -> Parser String
symbol = L.symbol skipWS

betweenSymbol :: String -> String -> Parser a -> Parser a
betweenSymbol l r = between (symbol l) (symbol r)

parseFun :: Parser HiValue
parseFun = lexeme $ do
  fun <- choice
    [ HiFunAdd            <$ string "add"
    , HiFunSub            <$ string "sub"
    , HiFunMul            <$ string "mul"
    , HiFunDiv            <$ string "div"
    , HiFunAnd            <$ string "and"
    , HiFunOr             <$ string "or"
    , HiFunLessThan       <$ string "less-than"
    , HiFunGreaterThan    <$ string "greater-than"
    , HiFunEquals         <$ string "equals"
    , HiFunNotLessThan    <$ string "not-less-than"
    , HiFunNotGreaterThan <$ string "not-greater-than"
    , HiFunNotEquals      <$ string "not-equals"
    , HiFunNot            <$ string "not"
    , HiFunIf             <$ string "if"
    , HiFunLength         <$ string "length"
    , HiFunToUpper        <$ string "to-upper"
    , HiFunToLower        <$ string "to-lower"
    , HiFunReverse        <$ string "reverse"
    , HiFunTrim           <$ string "trim"
    , HiFunList           <$ string "list"
    , HiFunRange          <$ string "range"
    , HiFunFold           <$ string "fold"
    , HiFunPackBytes      <$ string "pack-bytes"
    , HiFunUnpackBytes    <$ string "unpack-bytes"
    , HiFunZip            <$ string "zip"
    , HiFunUnzip          <$ string "unzip"
    , HiFunEncodeUtf8     <$ string "encode-utf8"
    , HiFunDecodeUtf8     <$ string "decode-utf8"
    , HiFunSerialise      <$ string "serialise"
    , HiFunDeserialise    <$ string "deserialise"
    , HiFunChDir          <$ string "cd"
    , HiFunMkDir          <$ string "mkdir"
    , HiFunRead           <$ string "read"
    , HiFunWrite          <$ string "write"
    , HiFunParseTime      <$ string "parse-time"
    , HiFunRand           <$ string "rand"
    , HiFunEcho           <$ string "echo"
    , HiFunCount          <$ string "count"
    , HiFunKeys           <$ string "keys"
    , HiFunValues         <$ string "values"
    , HiFunInvert         <$ string "invert" ]
  return . HiValueFunction $ fun

parseValue :: Parser HiExpr
parseValue = choice $ fmap try
  [ HiExprValue <$> parseNum
  , HiExprValue <$> parseBool
  , HiExprValue <$> parseFun
  , HiExprValue <$> parseNull
  , HiExprValue <$> parseString
  , HiExprValue <$> parseAction
  , betweenSymbol "(" ")" parseExpr
  , parseList
  , parseByteList
  , parseDict ]

parseNum :: Parser HiValue
parseNum = lexeme $ HiValueNumber . toRational <$> L.signed skipWS L.scientific

parseBool :: Parser HiValue
parseBool = lexeme $ do
  fun <- choice
    [ True  <$ string "true"
    , False <$ string "false" ]
  return . HiValueBool $ fun

parseNull :: Parser HiValue
parseNull = HiValueNull <$ symbol "null"

parseString :: Parser HiValue
parseString = HiValueString . T.pack <$> lexeme ( char '"' >> manyTill L.charLiteral (char '"'))

parseArgsImpl :: Parser [HiExpr]
parseArgsImpl = parseExpr `sepBy` symbol ","

parseList :: Parser HiExpr
parseList = do HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> parseListLiteral
  where parseListLiteral = betweenSymbol "[" "]" parseArgsImpl

strToHex :: String -> Maybe Word8
strToHex [a, b] = case (toInt a, toInt b) of
  (Just aa, Just bb) -> Just (fromIntegral $ aa * 16 + bb)
  _                  -> Nothing
  where
    toInt n
      | isDigit n = Just (ord n - ord '0')
      | n `elem` "abcdef" = Just (ord n - ord 'a' + 10)
      | otherwise = Nothing
strToHex _ = Nothing

parseHex :: Parser Word8
parseHex = lexeme $ do
  a <- count 2 hexDigitChar
  case strToHex a of
    Just r  -> return r
    Nothing -> fail "hex expected"

parseByteList :: Parser HiExpr
parseByteList = betweenSymbol "[#" "#]" (HiExprValue . HiValueBytes . B.pack <$> many parseHex)

parseAction :: Parser HiValue
parseAction = lexeme $ choice
  [ HiValueAction HiActionCwd <$ string "cwd"
  , HiValueAction HiActionNow <$ string "now" ]

parseDict :: Parser HiExpr
parseDict = betweenSymbol "{" "}" (HiExprDict <$> parseEntry `sepBy` symbol ",")
  where
    parseEntry = do
      key <- parseExpr
      _ <- symbol ":"
      value <- parseExpr
      return (key, value)

parseApplication :: Parser (HiExpr -> HiExpr)
parseApplication = betweenSymbol "(" ")" (do
  args <- parseArgsImpl
  return (`HiExprApply` args))

parseRunExpr :: Parser (HiExpr -> HiExpr)
parseRunExpr = HiExprRun <$ symbol "!"

parseDotAccess :: Parser (HiExpr -> HiExpr)
parseDotAccess = char '.' *> do
  arg <- parseDotAccessArg <$> parseDotString
  return (\val -> HiExprApply val [HiExprValue arg])
  where
    parseDotString = ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
    parseDotAccessArg = HiValueString . T.pack . intercalate "-"

indexAccessChain :: Operator Parser HiExpr
indexAccessChain = Postfix (wrapAppl <$> some (try parseApplication <|> try parseRunExpr <|> parseDotAccess))
  where
    wrapAppl fList val = foldr (\x y -> x y) val (reverse fList)

parseExpr :: Parser HiExpr
parseExpr = makeExprParser parseValue table
  where
    wrapBinary f a b = HiExprApply (HiExprValue (HiValueFunction f)) [a, b]
    ifxImpl ast s "" f = ast (wrapBinary f <$ try (symbol s))
    ifxImpl ast s nf f = ast (wrapBinary f <$ try (symbol s <* notFollowedBy (symbol nf)))
    lifx s f = ifxImpl InfixL s f
    rifx s f = ifxImpl InfixR s f
    nifx s f = ifxImpl InfixN s f
    table = [
      [indexAccessChain],
      [lifx "*" "" HiFunMul,             lifx "/" "=" HiFunDiv],
      [lifx "+" "" HiFunAdd,             lifx "-" "" HiFunSub],
      [nifx "==" "" HiFunEquals,         nifx "/=" "" HiFunNotEquals,
       nifx "<=" "" HiFunNotGreaterThan, nifx ">=" "" HiFunNotLessThan,
       nifx "<" "" HiFunLessThan,        nifx ">" "" HiFunGreaterThan],
      [rifx "&&" "" HiFunAnd],
      [rifx "||" "" HiFunOr]
      ]
