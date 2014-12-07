module Model ( LispVal(..)
             , true_p, self_evaluating_p, lisp_prim
             , Environment
             , Model.empty, Model.lookup, Model.fromList, bind, extend, arglist_env) where

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

data Environment = Env Environment (Map String LispVal)
                 | None deriving (Show)

fromList :: Environment -> [(String, LispVal)] -> Environment
fromList env tbl = Env env $ Data.Map.fromList tbl

empty :: Environment
empty = Env None Data.Map.empty

lookup :: Environment -> String -> LispVal
lookup None _ = Nil
lookup (Env prev env) k = case Data.Map.lookup k env of
                            Nothing -> Model.lookup prev k
                            Just v -> v

bind :: Environment -> String -> LispVal -> Environment
bind None _ _ = None
bind (Env prev env) k v = Env prev $ insert k v env

extend :: Environment -> Environment
extend env = Env env $ Data.Map.empty

arglist_env :: Environment -> LispVal -> LispVal -> Environment
arglist_env env Nil Nil = env
arglist_env env (Cell (Sym k) ks) (Cell v vs) = arglist_env (bind env k v) ks vs
arglist_env _ _ Nil = error "Too few arguments..."
arglist_env _ Nil _ = error "Too many arguments..."
arglist_env env ks vs = error . unlines $ ["Something odd happened", show ks, show vs, show env]

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
