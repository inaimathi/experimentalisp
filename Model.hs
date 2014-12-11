module Model ( LispVal(..)
             , true_p, self_evaluating_p, lisp_prim
             , Environment, global_env
             , Model.lookup, bind, extend, arglist_env) where

import Data.Map hiding (map)

data LispVal = Cell LispVal LispVal
             | Str String
             | Sym String
             | Num Integer
             | Bool Bool
             | Chr Char 
             | Nil
             | Primitive (Environment -> (LispVal, Environment)) LispVal
             | Procedure Environment LispVal LispVal
             | Fexpr Environment LispVal LispVal

instance Eq LispVal where
    (Str a) == (Str b) = a == b
    (Num a) == (Num b) = a == b
    (Sym a) == (Sym b) = a == b
    (Chr a) == (Chr b) = a == b
    (Bool a) == (Bool b) = a == b
    (Cell car cdr) == (Cell car' cdr') = car == car' && cdr == cdr'
    Nil == Nil = True
    _ == _ = False
    

instance Show LispVal where
    show (Str s) = show s
    show (Sym s) = s
    show (Num n) = show n
    show cell@(Cell _ _) = concat $ "(" : (recur cell)
        where recur (Cell car Nil) = [show car, ")"]
              recur (Cell car cdr@(Cell _ _)) = show car : " " : (recur cdr)
              recur (Cell car cdr) = [show car, " . ", show cdr, ")"]
              recur _ = error "Invalid form in cell recur"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (Chr c) = "#" ++ show c
    show Nil = "NIL"
    show (Primitive _ args) = "<prim " ++ show args ++ ">"
    show (Procedure _ args _) = "<fn " ++ show args ++ ">"
    show (Fexpr _ args _) = "<fexpr " ++ show args ++ ">"

lisp_prim :: [String] -> (Environment -> [LispVal] -> (LispVal, Environment)) -> LispVal
lisp_prim args fn = Primitive (\env -> fn env $ map (Model.lookup env) args) $ argl args
    where argl [] = Nil
          argl (a:rest) = Cell (Sym a) $ argl rest

type Environment = [Map String LispVal]

lookup :: Environment -> String -> LispVal
lookup [] _ = Nil
lookup (env:rest) k = case Data.Map.lookup k env of
                        Nothing -> Model.lookup rest k
                        Just v -> v

bind :: Environment -> String -> LispVal -> Environment
bind [] _ _ = []
bind (env:rest) k v = (insert k v env) : rest

extend :: Environment -> Environment
extend env = empty:env

arglist_env :: Environment -> LispVal -> LispVal -> Environment
arglist_env env Nil Nil = env
arglist_env env (Cell (Sym k) ks) (Cell v vs) = arglist_env (bind env k v) ks vs
-- arglist_env _ _ Nil = error "Too few arguments..."
-- arglist_env _ Nil _ = error "Too many arguments..."
-- arglist_env env ks vs = error . unlines $ ["Something odd happened", show ks, show vs, show env]
arglist_env env _ _ = env

true_p :: LispVal -> Bool
true_p (Bool False) = False
true_p _ = True

self_evaluating_p :: LispVal -> Bool
self_evaluating_p (Bool _) = True
self_evaluating_p Nil = True
self_evaluating_p (Str _) = True
self_evaluating_p (Num _) = True
self_evaluating_p (Chr _) = True
self_evaluating_p _ = False

lisp_env :: Environment -> LispVal
lisp_env [] = Nil
lisp_env (frame:rest) = Cell (map_to_conses frame) $ lisp_env rest
    where map_to_conses frame = recur $ toList frame
              where recur ((k, v):rest) = Cell (Cell (Sym k) v) $ recur rest
                    recur [] = Nil


global_env :: Environment
global_env = [fromList 
              [ ("+", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a + b, env)))
              , ("-", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a - b, env)))
              , ("/", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a `div` b, env)))
              , ("*", lisp_prim ["a", "b"] (\env [Num a, Num b] -> (Num $ a * b, env)))
              , ("=", lisp_prim ["a", "b"] (\env [a, b] -> (Bool $ a == b, env)))
              , ("car", lisp_prim ["a"] (\env [Cell car _] -> (car, env)))
              , ("cdr", lisp_prim ["a"] (\env [Cell _ cdr] -> (cdr, env)))
              , ("cons", lisp_prim ["a", "b"] (\env [a, b] -> (Cell a b, env)))
              , ("the-env", lisp_prim ["a", "b"] (\env _ -> (lisp_env env, env)))
             ]]
