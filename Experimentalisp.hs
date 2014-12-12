module Main where

import Model
import Reader
import Evaluator

import Pipes
import Control.Monad (unless, when)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import System.IO
import Data.Char (isSpace)

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
            str <- lift getLine
            yield str
            stdinLn

reader :: (Monad m) => [String] -> Pipe String LispVal m ()
reader acc = do ln <- await
--                when ("" == ln) $ yield $ Sym "EOL"
--                yield $ Str ln
                case strip ln of
                  ":c" -> reader []
                  _ -> case lisp_read . unlines $ reverse (ln:acc) of
                         Right res -> do yield res
                                         reader []
                         Left _ -> reader $ ln : acc

evaluator :: (Monad m) => Environment -> Pipe LispVal LispVal m ()
evaluator env = do exp <- await
                   let evaled = eval exp env
                       env' = case evaled of
                                (Res _) -> env
                                (Mod _ e) -> e
                   do yield $ res_of evaled
                      evaluator env'

prompt :: Show a => Consumer a IO ()
prompt = do lift $ putStr "\nEXP>> "
            lift $ hFlush stdout
            msg <- await
            x <- lift $ try $ putStrLn $ show msg
            case x of
              Left e@(G.IOError { G.ioe_type = t}) ->
                  lift $ unless (t == G.ResourceVanished) $ throwIO e
              Right () -> prompt
                    

toStdout :: Show a => Consumer a IO ()
toStdout = do
  msg <- await
  x   <- lift $ try $ putStr $ show msg
  case x of
    Left e@(G.IOError { G.ioe_type = t}) ->
           lift $ unless (t == G.ResourceVanished) $ throwIO e
    Right () -> do lift $ hFlush stdout
                   toStdout

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          runEffect $ stdinLn >-> 
                    reader [] >-> evaluator global_env >-> 
                    prompt
