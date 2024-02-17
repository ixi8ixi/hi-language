{-# LANGUAGE DerivingVia #-}
module HW5.Action
  ( HiPermission(..)
  , HIO(..)
  , PermissionException(..)) where

import           Control.Exception
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString            as BS
import qualified          Data.Sequence as Seq
import           Data.Set
import           Data.String                (IsString (fromString))
import           Data.Text                  (pack, unpack)
import           Data.Text.Encoding         (decodeUtf8')
import           Data.Time                  (getCurrentTime)
import           HW5.Base
import           System.Directory           (createDirectoryIfMissing,
                                             doesDirectoryExist, doesFileExist,
                                             getCurrentDirectory, listDirectory,
                                             setCurrentDirectory)
import           System.Random              (getStdRandom, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Show)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving (Eq, Ord, Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving (Functor, Applicative, Monad) via ReaderT (Set HiPermission) IO

instance MonadIO HIO where
  liftIO a = HIO (const a)

instance HiMonad HIO where
  runAction HiActionCwd = checkPermission AllowRead (do
    path <- liftIO getCurrentDirectory
    return . HiValueString . pack $ path)
  runAction (HiActionChDir path) = checkPermission AllowRead (do
    liftIO . setCurrentDirectory $ path
    return HiValueNull)
  runAction (HiActionMkDir path) = checkPermission AllowWrite (do
    liftIO . createDirectoryIfMissing True $ path
    return HiValueNull)
  runAction (HiActionRead path) = checkPermission AllowRead (do
    isDir <- liftIO . doesDirectoryExist $ path
    if isDir
      then readD path
      else do
        isFile <- liftIO . doesFileExist $ path
        if isFile
          then readF path
          else return HiValueNull)
  runAction (HiActionWrite path bs) = checkPermission AllowWrite (do
    liftIO $ BS.writeFile path bs
    return HiValueNull)
  runAction HiActionNow = checkPermission AllowTime (do
    cTime <- liftIO getCurrentTime
    return . HiValueTime $ cTime)
  runAction (HiActionRand f t) = do
    result <- getStdRandom (uniformR (f, t))
    return . HiValueNumber . toRational $ result
  runAction (HiActionEcho txt) = do
    liftIO . putStrLn . unpack $ txt
    return HiValueNull

checkPermission :: HiPermission -> HIO b -> HIO b
checkPermission permission f = do
    pSet <- HIO return
    if member permission pSet
      then f
      else liftIO . throwIO . PermissionRequired $ permission

readD :: FilePath -> HIO HiValue
readD path = do
  contents <- liftIO . listDirectory $ path
  return . HiValueList . Seq.fromList . fmap (HiValueString . fromString) $ contents

readF :: FilePath -> HIO HiValue
readF path = do
  contents <- liftIO . BS.readFile $ path
  case decodeUtf8' contents of
    Left _    -> return . HiValueBytes $ contents
    Right txt -> return . HiValueString $ txt
