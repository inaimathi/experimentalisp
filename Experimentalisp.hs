module Main where

import Model
import Reader
import Evaluator

import Pipes
import Control.Monad (unless)
import System.IO
import Data.Char (isSpace)

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          runEffect $ stdinLn >-> reader >-> evaluator >-> prompt

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
            str <- lift getLine
            yield str
            stdinLn

reader :: (Monad m) => Pipe String LispVal m ()
reader = loop []
    where loop acc = do 
            ln <- await
            case strip ln of
              ":c" -> loop []
              _ -> case lisp_read . unlines $ reverse (ln:acc) of
                     Right res -> do yield res
                                     loop []
                     Left _ -> loop $ ln : acc

evaluator :: (Monad m) => Pipe LispVal LispVal m ()
evaluator = loop global_env
    where loop env = do 
            exp <- await
            let evaled = eval exp env
                env' = case evaled of
                         (Res _) -> env
                         (Mod _ e) -> e
            do yield $ res_of evaled
               loop env'

prompt :: Show a => Consumer a IO ()
prompt = do lift $ putStr "\nEXP>> "
            lift $ hFlush stdout
            msg <- await
            lift $ putStrLn $ show msg
            prompt

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace
