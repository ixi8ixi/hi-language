{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
module HW5.Evaluator
  ( eval
  ) where

import qualified Codec.Compression.Zlib     as Z
import           Codec.Serialise            (deserialise, serialise)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Foldable              (Foldable (foldl', toList))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             (stimes)
import qualified Data.Sequence              as S
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8', encodeUtf8)
import           Data.Time                  (UTCTime, addUTCTime, diffUTCTime)
import           Data.Word                  (Word8)
import           GHC.Real                   (Ratio ((:%)))
import           HW5.Base
import           Prelude                    hiding (drop, take)
import           Text.Read                  (readMaybe)

class Semigroup a => ListLike a where
  lstLen :: a -> Int
  actual :: a -> Int -> Int
  take :: Int -> a -> a
  drop :: Int -> a -> a
  index :: a -> Int -> Maybe HiValue
  slice :: a -> Int -> Int -> a

  actual lst idx = let
    len = lstLen lst
    in if idx < 0
      then len + idx
      else idx
  slice sd from to = let
    af = actual sd from
    at = actual sd to
    in drop af (take at sd)

instance ListLike T.Text where
  lstLen = T.length
  take = T.take
  drop = T.drop
  index txt n = case T.uncons (slice txt n (n + 1)) of
    Just (c, _) -> Just . HiValueString . T.singleton $ c
    Nothing     -> Nothing

instance ListLike (S.Seq HiValue) where
  lstLen = S.length
  take = S.take
  drop = S.drop
  index s n = S.lookup n s

instance ListLike BS.ByteString where
  lstLen = BS.length
  take = BS.take
  drop = BS.drop
  index s n = case BS.uncons (slice s n (n + 1)) of
    Just (c, _) -> Just . HiValueNumber . toRational $ c
    Nothing     -> Nothing

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalImpl expr

evalImpl :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalImpl (HiExprValue value) = return value
evalImpl (HiExprApply (HiExprValue val) args) =
  case val of
    (HiValueFunction fun) -> applyFun fun args
    str@(HiValueString _) -> applyContainer str args
    l@(HiValueList _)     -> applyContainer l args
    b@(HiValueBytes _)    -> applyContainer b args
    dict@(HiValueDict _)  -> applyDict dict args
    _                     -> throwE HiErrorInvalidFunction
evalImpl (HiExprApply appl args) = do
  cappl <- HiExprValue <$> evalImpl appl
  evalImpl (HiExprApply cappl args)
evalImpl (HiExprRun a) = do
  arg <- evalImpl a
  case arg of
    (HiValueAction action) -> lift . runAction $ action
    _                      -> throwE HiErrorInvalidArgument
evalImpl (HiExprDict el) = do
  entryList <- evalKeyValue el
  return . HiValueDict . M.fromList $ entryList

evalEntry :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalEntry (key, value) = do
  eKey <- evalImpl key
  eValue <- evalImpl value
  return (eKey, eValue)

evalKeyValue :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
evalKeyValue = mapM evalEntry

applyFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
applyFun HiFunAdd            = binary cAdd
applyFun HiFunSub            = binary cSub
applyFun HiFunMul            = binary cMul
applyFun HiFunDiv            = binary cDiv
applyFun HiFunNot            = unary cNot
applyFun HiFunAnd            = cLazyAnd
applyFun HiFunOr             = cLazyOr
applyFun HiFunLessThan       = binary cLessThan
applyFun HiFunGreaterThan    = binary cGreaterThan
applyFun HiFunEquals         = binary cEquals
applyFun HiFunNotLessThan    = binary cNotLessThan
applyFun HiFunNotGreaterThan = binary cNotGreaterThan
applyFun HiFunNotEquals      = binary cNotEquals
applyFun HiFunIf             = cLazyIf
applyFun HiFunLength         = unary cListLen
applyFun HiFunToUpper        = unary cToUpper
applyFun HiFunToLower        = unary cToLower
applyFun HiFunReverse        = unary cReverse
applyFun HiFunTrim           = unary cTrim
applyFun HiFunList           = varar cList
applyFun HiFunRange          = binary cRange
applyFun HiFunFold           = binary cFold
applyFun HiFunPackBytes      = unary cPackBytes
applyFun HiFunUnpackBytes    = unary cUnpackBytes
applyFun HiFunEncodeUtf8     = unary cEncodeUtf8
applyFun HiFunDecodeUtf8     = unary cDecodeUtf8
applyFun HiFunZip            = unary cZip
applyFun HiFunUnzip          = unary cUnzip
applyFun HiFunSerialise      = unary cSerialize
applyFun HiFunDeserialise    = unary cDeserialize
applyFun HiFunChDir          = unary cChangeDir
applyFun HiFunMkDir          = unary cMakeDir
applyFun HiFunRead           = unary cRead
applyFun HiFunWrite          = binary cWrite
applyFun HiFunParseTime      = unary cParseTime
applyFun HiFunRand           = binary cRand
applyFun HiFunEcho           = unary cEcho
applyFun HiFunKeys           = unary cKeys
applyFun HiFunValues         = unary cValues
applyFun HiFunInvert         = unary cInvert
applyFun HiFunCount          = unary cCount

applyContainer :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyContainer str args@[_]    = binary cAbstrLookup (HiExprValue str:args)
applyContainer str args@[_, _] = ternary cContainerSlice (HiExprValue str:args)
applyContainer _ _             = throwE HiErrorArityMismatch

applyDict :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
applyDict dict args@[_] = binary cDictLookup (HiExprValue dict:args)
applyDict _ _           = throwE HiErrorArityMismatch

unary ::  HiMonad m => (HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
unary f [a] = do
  aa <- evalImpl a
  f aa
unary _ _ = throwE HiErrorArityMismatch

binary :: HiMonad m => (HiValue -> HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
binary f [a, b] = do
  aa <- evalImpl a
  bb <- evalImpl b
  f aa bb
binary _ _  = throwE HiErrorArityMismatch

ternary :: HiMonad m => (HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
ternary f [a, b, c] = do
  aa <- evalImpl a
  bb <- evalImpl b
  cc <- evalImpl c
  f aa bb cc
ternary _ _  = throwE HiErrorArityMismatch

varar :: HiMonad m => ([HiValue] -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
varar f args = do
  eArgs <- mapM evalImpl args
  f eArgs

cDictLookup :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cDictLookup (HiValueDict m) key = return $ fromMaybe HiValueNull (M.lookup key m)
cDictLookup _ _ = throwE HiErrorInvalidArgument

basicFunImpl :: HiMonad m => (Rational -> Rational -> Rational) -> HiValue -> HiValue -> ExceptT HiError m HiValue
basicFunImpl f (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber (f a b)
basicFunImpl _ _ _                                 = throwE HiErrorInvalidArgument

cAdd :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cAdd (HiValueTime time) (HiValueNumber add) = return . HiValueTime $ addUTCTime (fromRational add) time
cAdd (HiValueBytes a) (HiValueBytes b)      = return . HiValueBytes $ a <> b
cAdd (HiValueList a) (HiValueList b)        = return . HiValueList $ a S.>< b
cAdd (HiValueString a) (HiValueString b)    = return . HiValueString $ a `T.append` b
cAdd a b                                    = basicFunImpl (+) a b

cSub :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cSub (HiValueTime a) (HiValueTime b) = return . HiValueNumber . toRational $ diffUTCTime a b
cSub a b = basicFunImpl (-) a b

mulImpl :: (Monad m, Semigroup a) => (a -> HiValue) -> Rational -> a -> ExceptT HiError m HiValue
mulImpl valc (n :% d) sd = if d == 1
    then return . valc $ stimes n sd
    else throwE HiErrorInvalidArgument

cMul :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cMul (HiValueBytes bytes) (HiValueNumber num) = mulImpl HiValueBytes num bytes
cMul (HiValueList lst) (HiValueNumber num)    = mulImpl HiValueList num lst
cMul (HiValueString txt) (HiValueNumber num)  = mulImpl HiValueString num txt
cMul a b                                      = basicFunImpl (*) a b

cDiv :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cDiv (HiValueString a) (HiValueString b) = return . HiValueString $ (a `T.append` T.singleton '/' `T.append` b)
cDiv (HiValueNumber _) (HiValueNumber 0) = throwE HiErrorDivideByZero
cDiv a b                                 = basicFunImpl (/) a b

cNot :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cNot (HiValueBool a) = return $ HiValueBool (not a)
cNot _               = throwE HiErrorInvalidArgument

cLazyAnd :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
cLazyAnd [a, b] = do
  aa <- evalImpl a
  case aa of
    t@(HiValueBool False) -> return t
    t@HiValueNull         -> return t
    _                     -> evalImpl b
cLazyAnd _ = throwE HiErrorArityMismatch

cLazyOr :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
cLazyOr [a, b] = do
  aa <- evalImpl a
  case aa of
    (HiValueBool False) -> evalImpl b
    HiValueNull         -> evalImpl b
    _                   -> return aa
cLazyOr _ = throwE HiErrorArityMismatch

applyNotOnHiValue :: HiMonad m => (HiValue -> HiValue -> ExceptT HiError m HiValue) -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyNotOnHiValue f a b = do
  val <- f a b
  cNot val

cLessThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cLessThan a b = return . HiValueBool $ a < b

cGreaterThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cGreaterThan a b = cLessThan b a

cNotLessThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cNotLessThan = applyNotOnHiValue cLessThan

cNotGreaterThan :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cNotGreaterThan = applyNotOnHiValue cGreaterThan

cEquals :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cEquals a b = return . HiValueBool $ a == b

cNotEquals :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cNotEquals = applyNotOnHiValue cEquals

cLazyIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
cLazyIf [condition, a, b] = do
  condEvaluated <- evalImpl condition
  case condEvaluated of
    (HiValueBool True)  -> evalImpl a
    (HiValueBool False) -> evalImpl b
    _                   -> throwE HiErrorInvalidArgument
cLazyIf _ = throwE HiErrorArityMismatch

lstLenValue :: (Monad m, ListLike a) => a -> m HiValue
lstLenValue = return . HiValueNumber . toRational . lstLen

cListLen :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cListLen (HiValueList lst)    = lstLenValue lst
cListLen (HiValueString str)  = lstLenValue str
cListLen (HiValueBytes bytes) = lstLenValue bytes
cListLen _                    = throwE HiErrorInvalidArgument

transformString :: HiMonad m => HiValue -> (T.Text -> T.Text) -> ExceptT HiError m HiValue
transformString (HiValueString txt) f = return . HiValueString . f $ txt
transformString _ _                   = throwE HiErrorInvalidArgument

cToUpper :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cToUpper str = transformString str T.toUpper

cToLower :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cToLower str = transformString str T.toLower

cTrim :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cTrim str = transformString str T.strip

cReverse :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cReverse (HiValueBytes bytes) = return . HiValueBytes . BS.reverse $ bytes
cReverse (HiValueList l)      = return . HiValueList . S.reverse $ l
cReverse str                  = transformString str T.reverse

lookupImpl :: (ListLike a, HiMonad m) => a -> HiValue -> ExceptT HiError m HiValue
lookupImpl lst (HiValueNumber (a :% b)) = if b == 1
  then let
    result = index lst (fromInteger a)
  in case result of
    Just r  -> return r
    Nothing -> return HiValueNull
  else throwE HiErrorInvalidArgument
lookupImpl _ _ = throwE HiErrorInvalidArgument

cAbstrLookup :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cAbstrLookup (HiValueString txt) num = lookupImpl txt num
cAbstrLookup (HiValueList txt) num   = lookupImpl txt num
cAbstrLookup (HiValueBytes txt) num  = lookupImpl txt num
cAbstrLookup _ _                     = throwE HiErrorInvalidArgument

sliceImpl :: (ListLike a, HiMonad m) => (a -> HiValue) -> a -> HiValue -> HiValue -> ExceptT HiError m HiValue
sliceImpl constr lst (HiValueNumber (a :% b)) (HiValueNumber (c :% d)) = if b == 1 && d == 1
  then return . constr $ slice lst (fromInteger a) (fromInteger c)
  else throwE HiErrorInvalidArgument
sliceImpl _ _ _ _ = throwE HiErrorInvalidArgument

cContainerSlice :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
cContainerSlice txt HiValueNull b = cContainerSlice txt (HiValueNumber 0) b
cContainerSlice lst a HiValueNull = do
  len <- cListLen lst
  cContainerSlice lst a len
cContainerSlice (HiValueString txt) a b = sliceImpl HiValueString txt a b
cContainerSlice (HiValueList txt) a b = sliceImpl HiValueList txt a b
cContainerSlice (HiValueBytes txt) a b = sliceImpl HiValueBytes txt a b
cContainerSlice _ _ _ = throwE HiErrorInvalidArgument

cList :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
cList = return . HiValueList . S.fromList

cRange :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cRange (HiValueNumber from) (HiValueNumber to) = return . HiValueList . S.fromList $ fmap HiValueNumber [from..to]
cRange _ _ = throwE HiErrorInvalidArgument

cFold :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cFold (HiValueFunction fun) (HiValueList args) = case toList args of
  [] -> return HiValueNull
  f:lst -> foldl' (\x y -> do
    a <- x
    b <- y
    applyFun fun [HiExprValue a, HiExprValue b] ) (return f) (fmap return lst)
cFold _ _ = throwE HiErrorInvalidArgument

cToByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
cToByte (HiValueNumber (a :% b)) = if b == 1 && a >= 0 && a < 256
  then return . fromIntegral $ a
  else throwE HiErrorInvalidArgument
cToByte _ = throwE HiErrorInvalidArgument

cPackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cPackBytes (HiValueList s) = do
  res <- mapM cToByte s
  let lst = toList res
  return . HiValueBytes . BS.pack $ lst
cPackBytes _ = throwE HiErrorInvalidArgument

cUnpackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cUnpackBytes (HiValueBytes b) = return . HiValueList . fmap (HiValueNumber . toRational) . S.fromList . BS.unpack $ b
cUnpackBytes _ = throwE HiErrorInvalidArgument

cEncodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cEncodeUtf8 (HiValueString txt) = return . HiValueBytes . encodeUtf8 $ txt
cEncodeUtf8 _                   = throwE HiErrorInvalidArgument

cDecodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cDecodeUtf8 (HiValueBytes b) = case decodeUtf8' b of
  Left _    -> return HiValueNull
  Right txt -> return . HiValueString $ txt
cDecodeUtf8 _ = throwE HiErrorInvalidArgument

cZip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cZip (HiValueBytes bytes) = return . HiValueBytes . toStrict .
  Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression } . fromStrict $ bytes
cZip _ = throwE HiErrorInvalidArgument

cUnzip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cUnzip (HiValueBytes bytes) = return . HiValueBytes . toStrict . Z.decompress . fromStrict $ bytes
cUnzip _ = throwE HiErrorInvalidArgument

cSerialize :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cSerialize = return . HiValueBytes . toStrict . serialise

cDeserialize :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cDeserialize (HiValueBytes bytes) = return . deserialise . fromStrict $ bytes
cDeserialize _                    = throwE HiErrorInvalidArgument

packAction :: HiMonad m => HiValue -> (String -> HiAction) -> ExceptT HiError m HiValue
packAction (HiValueString path) act = return . HiValueAction . act . T.unpack $ path
packAction _ _ = throwE HiErrorInvalidArgument

cChangeDir :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cChangeDir path = packAction path HiActionChDir

cMakeDir :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cMakeDir path = packAction path HiActionMkDir

cRead :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cRead path = packAction path HiActionRead

cWrite :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cWrite (HiValueString path) (HiValueBytes content) = (return . HiValueAction) $ HiActionWrite (T.unpack path) content
cWrite path (HiValueString str) = cWrite path (HiValueBytes . encodeUtf8 $ str)
cWrite _ _ = throwE HiErrorInvalidArgument

cParseTime :: Monad m => HiValue -> ExceptT HiError m HiValue
cParseTime (HiValueString str) = case readMaybe (T.unpack str) :: Maybe UTCTime of
  Just time -> return . HiValueTime $ time
  Nothing   -> return HiValueNull
cParseTime _ = throwE HiErrorInvalidArgument

cRand :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
cRand (HiValueNumber (a :% b)) (HiValueNumber (c :% d)) = if b == 1 && d == 1
  then return . HiValueAction $ HiActionRand (fromInteger a) (fromInteger c)
  else throwE HiErrorInvalidArgument
cRand _ _ = throwE HiErrorInvalidArgument

cEcho :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cEcho (HiValueString str) = return . HiValueAction . HiActionEcho $ str
cEcho _                   = throwE HiErrorInvalidArgument

cDictSets :: HiMonad m => HiValue -> (M.Map HiValue HiValue -> [HiValue]) -> ExceptT HiError m HiValue
cDictSets (HiValueDict m) f = return . HiValueList . S.fromList . f $ m
cDictSets _ _               = throwE HiErrorInvalidArgument

cKeys :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cKeys dict = cDictSets dict M.keys

cValues :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cValues dict = cDictSets dict (fmap snd . M.assocs)

cInvert :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cInvert (HiValueDict m) = let
  invEntryList = fmap (\(key, value) -> (value, [key])) . M.toList $ m
  in return . HiValueDict . M.map (HiValueList . S.fromList) . M.fromListWith (++) $ invEntryList
cInvert _ = throwE HiErrorInvalidArgument

countImpl :: Ord k1 => (k1 -> HiValue) -> [k1] -> HiValue
countImpl f = HiValueDict . M.mapKeys f . M.map HiValueNumber . M.fromListWith (+) . fmap (, 1)

cCount :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cCount (HiValueString txt) = return . countImpl (HiValueString . T.singleton) . T.unpack $ txt
cCount (HiValueBytes bs)   = return . countImpl (HiValueNumber . toRational) . BS.unpack $ bs
cCount (HiValueList lst)   = return . HiValueDict . M.map HiValueNumber . M.fromListWith (+) . fmap (, 1) . toList $ lst
cCount _ = throwE HiErrorInvalidArgument
