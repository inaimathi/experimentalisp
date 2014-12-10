module Main where

import Model
import Reader
import Evaluator

main_loop :: Environment -> IO ()
main_loop env = do putStr " >> "
                   ln <- getLine
                   case lisp_read ln of
                     Right res -> let (evaled, env') = eval res env
                                  in do putStrLn $ show evaled
                                        main_loop env'
                     _ -> do putStrLn "READ ERROR"
                             main_loop env

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          main_loop global_env
