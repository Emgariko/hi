module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT, outputStr )
import HW3.Parser (parse)
import Text.Megaparsec (ShowErrorComponent(showErrorComponent), errorBundlePretty)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import HW3.Action (HiPermission(..))

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       perms = fromList [AllowRead, AllowWrite]
       loop = do
            minput <- getInputLine "hi>"
            case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                    (case parse input of
                        Left e -> outputStrLn $ errorBundlePretty e
                        Right r -> do
                            -- res <- eval r
                            res <- liftIO $ runHIO $ eval r $ perms
                            case res of 
                                (Left err) -> outputStrLn $ show err
                                (Right val) -> outputStrLn $ show (prettyValue val))
                    loop
                