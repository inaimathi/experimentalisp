module Model ( LispVal(..)
             , traverse, true_p, self_evaluating_p
             , Environment, Model.lookup, bind, extend_env) where

import Data.Map

data LispVal = Cell LispVal LispVal
             | Str String
             | Sym String
             | Num Integer
             | Bool Bool
             | Chr Char 
             | Nil
             | Primitive (Environment -> LispVal) LispVal
             | Procedure Environment LispVal LispVal
             | Fexpr Environment LispVal LispVal

instance Show LispVal where
    show (Str s) = show s
    show (Sym s) = s
    show (Num n) = show n
    show cell@(Cell _ _) = concat $ "(" : (recur cell)
        where recur (Cell car Nil) = [show car, ")"]
              recur (Cell car cdr@(Cell _ _)) = show car : " " : (recur cdr)
              recur (Cell car cdr) = [show car, " . ", show cdr]
              recur _ = error "Invalid form in cell recur"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (Chr c) = "#" ++ show c
    show Nil = "NIL"
    show (Primitive _ args) = "<prim " ++ show args ++ " >"
    show (Procedure _ args _) = "<fn " ++ show args ++ " >"
    show (Fexpr _ args _) = "<fexpr " ++ show args ++ " >"

traverse :: (LispVal -> LispVal) -> LispVal -> LispVal
traverse fn (Cell car cdr) = Cell (fn car) (fn cdr)
traverse fn val = fn val

data Environment = Env Environment (Map String LispVal)
                 | None

lookup :: Environment -> String -> LispVal
lookup None _ = Nil
lookup (Env prev env) k = case Data.Map.lookup k env of
                            Nothing -> Model.lookup prev k
                            Just v -> v

bind :: Environment -> String -> LispVal -> Environment
bind None _ _ = None
bind (Env prev env) k v = Env prev $ insert k v env

extend_env :: Environment -> LispVal -> LispVal -> Environment
extend_env env Nil Nil = env
extend_env env (Cell (Sym k) ks) (Cell v vs) = extend_env (bind env k v) ks vs
extend_env _ _ Nil = error "Too few arguments..."
extend_env _ Nil _ = error "Too many arguments..."

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
