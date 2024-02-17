module HW5.Pretty
  ( prettyValue
  ) where

import           Prettyprinter                 (Doc, Pretty (pretty), comma,
                                                hsep, punctuate, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)

import           Data.Scientific
import           HW5.Base

import qualified Data.ByteString               as B
import           Data.Foldable                 (toList)
import qualified Data.Map                      as M
import           Data.Text                     (pack)
import           Data.Word                     (Word8)
import           GHC.Real                      (Ratio ((:%)))
import           Text.Printf                   (printf)

between :: String -> String -> Doc AnsiStyle -> Doc AnsiStyle
between l r doc = pretty l <> doc <> pretty r

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction f) = function f
prettyValue (HiValueNumber n)   = number n
prettyValue (HiValueBool b)     = bool b
prettyValue (HiValueString txt) = between "\"" "\"" (pretty txt)
prettyValue HiValueNull         = pretty "null"
prettyValue (HiValueList s)     = between "[ " " ]" (hsep . punctuate comma . toList . fmap prettyValue $ s)
prettyValue (HiValueBytes b)    = between "[# " " #]" (hsep . fmap showHex $ B.unpack b)
prettyValue (HiValueAction a)   = prettyAction a
prettyValue (HiValueTime t)     = between "parse-time(\"" "\")" (pretty . show $ t)
prettyValue (HiValueDict m)     = between "{ " " }" (hsep . punctuate comma . fmap (\(key, value) ->
  prettyValue key <+> pretty ":" <+> prettyValue value) . M.toList $ m)

toPrettyString :: String -> Doc AnsiStyle
toPrettyString = prettyValue . HiValueString . pack

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction HiActionCwd                 = pretty "cwd"
prettyAction (HiActionChDir dir)         = between "cd(" ")" (toPrettyString dir)
prettyAction (HiActionMkDir dir)         = between "mkdir(" ")" (toPrettyString dir)
prettyAction (HiActionRead dir)          = between "read(" ")" (toPrettyString dir)
prettyAction (HiActionWrite dir content) = between "write(" ")"
  (pretty dir <> pretty ", " <> (prettyValue . HiValueBytes $ content))
prettyAction HiActionNow                 = pretty "now"
prettyAction (HiActionRand a b)          = between "rand(" ")" (pretty a <> pretty ", " <> pretty b)
prettyAction (HiActionEcho echo)         = between "echo(\"" "\")" (pretty echo)

showHex :: Word8 -> Doc AnsiStyle
showHex w = pretty (printf "%02x" w :: String)

number :: Rational -> Doc AnsiStyle
number (a :% 1) = pretty a
number num@(a :% b) = let
  (sci, mb) = fromRationalRepetendUnlimited num
  in case mb of
    Nothing -> pretty $ formatScientific Fixed Nothing sci
    _ -> let
      (q, r) = quotRem a b
      in if q == 0
        then prettyFraction a b
        else pretty q <+> pretty (if r > 0 then "+" else "-") <+> prettyFraction (abs r) b

prettyFraction :: Integer -> Integer -> Doc AnsiStyle
prettyFraction a b = pretty a <> pretty "/" <> pretty b

function :: HiFun -> Doc AnsiStyle
function f = pretty $ case f of
  HiFunAdd            -> "add"
  HiFunSub            -> "sub"
  HiFunMul            -> "mul"
  HiFunDiv            -> "div"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunChDir          -> "cd"
  HiFunMkDir          -> "mkdir"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"

bool :: Bool -> Doc AnsiStyle
bool b = if b then pretty "true" else pretty "false"
