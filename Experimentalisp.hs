module Main where

import Model
import Reader
import Evaluator

global_env :: Environment
global_env = fromList empty 
             [ ("+", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a + b, env)))
             , ("-", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a - b, env)))
             , ("/", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a `div` b, env)))
             , ("*", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a * b, env)))
             , ("=", lisp_prim ["a", "b"] (\env [a, b] -> (Bool $ a == b, env)))
             , ("car", lisp_prim ["a"] (\env [Cell car _] -> (car, env)))
             , ("cdr", lisp_prim ["a"] (\env [Cell _ cdr] -> (cdr, env)))
             , ("cons", lisp_prim ["a", "b"] cons)]
    where cons env [a, b] = (Cell a b, env)
          cons env args = error $ unlines ["CONS: ", show env, show args]

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
