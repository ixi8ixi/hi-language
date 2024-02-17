module Main (main) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Set
import           HW5.Action
import           HW5.Evaluator            (eval)
import           HW5.Parser               (parse)
import           HW5.Pretty               (prettyValue)
import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "Hi!> "
            case minput of
                Nothing -> return ()
                Just "stop" -> return ()
                Just input -> do
                    let expr = parse input
                    case expr of
                        Left e -> do
                            outputStrLn $ "Parse error: " ++ show e
                        Right parsed -> do
                            value <- liftIO $ runHIO (eval parsed) (fromList [AllowRead, AllowWrite, AllowTime])
                            case value of
                                Left ee -> do outputStrLn $ "Eval error: " ++ show ee
                                Right result -> do outputStrLn $ show $ prettyValue result
                    loop
