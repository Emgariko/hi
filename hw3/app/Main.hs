module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set (fromList)
import HW3.Action (HiPermission (..), runHIO)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn,
                                 runInputT)
import Text.Megaparsec (ShowErrorComponent (showErrorComponent), errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       perms = fromList [AllowRead, AllowWrite, AllowTime]
       loop = do
            minput <- getInputLine "hi>"
            case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                    (case parse input of
                        Left e -> outputStrLn $ errorBundlePretty e
                        Right r -> do
                            res <- liftIO $ runHIO (eval r) perms
                            case res of
                                (Left err)  -> outputStrLn $ show err
                                (Right val) -> outputStrLn $ show (prettyValue val))
                    loop

