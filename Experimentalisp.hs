module Main where

import Model
import Reader
import Evaluator

import System.IO

main_loop :: Environment -> IO ()
main_loop env = do putStr "EXP>> "
                   hFlush stdout
                   ln <- getLine
                   case lisp_read ln of
                     Right res -> let evaled = eval res env
                                      env' = case evaled of
                                               (Res _) -> env
                                               (Mod _ e) -> e
                                  in do putStrLn . show $ res_of evaled
                                        main_loop env'
                     _ -> do putStrLn "READ ERROR"
                             main_loop env

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          main_loop global_env
