module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT, outputStr )
import HW3.Parser (parse)
import Text.Megaparsec (ShowErrorComponent(showErrorComponent))
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
            minput <- getInputLine "hi>"
            case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                    (case parse input of
                        Left e -> outputStrLn $ show e
                        Right r -> do 
                            res <- eval r
                            case res of 
                                (Left err) -> outputStrLn $ show err
                                (Right val) -> outputStrLn $ show (prettyValue val))
                    loop
                