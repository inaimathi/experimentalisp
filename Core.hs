module Core (global_env) where

import Reader
import Model

lisp_env :: Environment -> LispVal -- HACK ALERT; envionments should be lisp values to begin with
lisp_env [] = Nil
lisp_env ((env, err):rest) = Cell (Cell (map_to_conses env) (map_to_conses err)) $ lisp_env rest
    where map_to_conses frame = recur $ toList frame
              where recur ((k, v):rest) = Cell (Cell (Sym k) v) $ recur rest
                    recur [] = Nil

global_env :: Environment
global_env = fromList 
             [ ("+", lisp_prim ["a", "b"] (\_ [Num a, Num b] -> Num $ a + b))
             , ("-", lisp_prim ["a", "b"] (\_ [Num a, Num b] -> Num $ a - b))
             , ("/", lisp_prim ["a", "b"] (\_ [Num a, Num b] -> Num $ a `div` b))
             , ("*", lisp_prim ["a", "b"] (\_ [Num a, Num b] -> Num $ a * b))
             , ("=", lisp_prim ["a", "b"] (\_ [a, b] -> Bool $ a == b))
             , ("car", lisp_prim ["a"] (\_ [Cell car _] -> car))
             , ("cdr", lisp_prim ["a"] (\_ [Cell _ cdr] -> cdr))
             , ("cons", lisp_prim ["a", "b"] (\_ [a, b] -> Cell a b))


             , ("the-env", lisp_prim [] (\env _ -> lisp_env env))
             , ("read", lisp_prim ["exp"] (\_ [Str exp] -> case lisp_read exp of
                                                              Right val -> val
                                                              _ -> error $ "Invalid expression: '" ++ exp ++ "'..."))
             , ("write", lisp_prim ["exp"] (\_ [exp] -> Str $ show exp))


               
             , ("type", lisp_prim ["v"] (\_ [val] -> mk_type val))
             , ("type-of", lisp_prim ["thing"] (\_ [v] -> lisp_type v))
             -- type-check
             -- 
             ]
