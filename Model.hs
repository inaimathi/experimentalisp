module Model ( LispVal(..)
             , true_p, self_evaluating_p, lisp_prim, lisp_length, lisp_error, lisp_type, mk_type
             , Environment
             , Model.lookup, bind, extend, arglist_env
             , Model.fromList, Data.Map.toList) where

import Data.Map hiding (map)

data LispVal = Cell LispVal LispVal
             | Str String
             | Sym String
             | Num Integer
             | Bool Bool
             | Chr Char 
             | Nil
             | Type String
             | Primitive (Environment -> LispVal) LispVal
             | Procedure Environment LispVal LispVal
             | Fexpr Environment LispVal LispVal

lisp_type :: LispVal -> LispVal
lisp_type (Str _) = Type "string"
lisp_type (Sym _) = Type "symbol"
lisp_type (Num _) = Type "number"
lisp_type (Bool _) = Type "boolean"
lisp_type (Chr _) = Type "character"
lisp_type Nil = Nil
--- NEED TO FIX THESE THREE 
--- (callables should be compound types. 
---  Like Cells, but different meaning.)
lisp_type (Primitive _ _) = Type "function"
lisp_type (Procedure _ _ _) = Type "function"
lisp_type (Fexpr _ _ _) = Type "fexpr"
----------------------------
lisp_type (Type _) = Type "type"
lisp_type (Cell a b) = case collapsed b $ lisp_type a of
                         Just t -> t
                         Nothing -> naive
    where collapsed (Cell a Nil) t
              | lisp_type a == t = Just $ Cell t Nil
              | otherwise = Nothing
          collapsed (Cell a rest) t
              | lisp_type a == t = collapsed rest t
              | otherwise = Nothing
          collapsed _ _ = Nothing
          naive = (Cell (lisp_type a) (lisp_type b))

mk_type :: LispVal -> LispVal
mk_type (Sym v) = Type v
mk_type (Cell (Sym v) Nil) = (Cell (Type v) Nil)
mk_type (Cell a b) = (Cell (mk_type a) (mk_type b))
mk_type Nil = Nil        

instance Eq LispVal where
    (Str a) == (Str b) = a == b
    (Num a) == (Num b) = a == b
    (Sym a) == (Sym b) = a == b
    (Chr a) == (Chr b) = a == b
    (Bool a) == (Bool b) = a == b
    (Type t) == (Type t') = t == t'
    (Cell car cdr) == (Cell car' cdr') = car == car' && cdr == cdr'
    Nil == Nil = True
    _ == _ = False
    

instance Show LispVal where
    show (Str s) = show s
    show (Sym s) = s
    show (Type t) = "::" ++ t
    show (Num n) = show n
    show cell@(Cell _ _) = concat $ "(" : (recur cell)
        where recur (Cell car Nil) = [show car, ")"]
              recur (Cell car cdr@(Cell _ _)) = show car : " " : (recur cdr)
              recur (Cell car cdr) = [show car, " . ", show cdr, ")"]
              recur _ = error "Invalid form in cell recur"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (Chr c) = ['\\', c]
    show Nil = "NIL"
    show (Primitive _ args) = "<prim " ++ show args ++ ">"
    show (Procedure _ args _) = "<fn " ++ show args ++ ">"
    show (Fexpr _ args _) = "<fexpr " ++ show args ++ ">"

lisp_prim :: [String] -> (Environment -> [LispVal] -> LispVal) -> LispVal
lisp_prim args fn = Primitive (\env -> fn env $ map (Model.lookup env) args) $ argl args
    where argl [] = Nil
          argl (a:rest) = Cell (Sym a) $ argl rest

type Tbl = Map String LispVal
type Environment = [(Tbl, Tbl)]

lookup :: Environment -> String -> LispVal
lookup [] _ = Nil
lookup ((env, _):rest) k = case Data.Map.lookup k env of
                             Nothing -> Model.lookup rest k
                             Just v -> v

bind :: Environment -> String -> LispVal -> Environment
bind [] _ _ = []
bind ((env, err):rest) k v = ((insert k v env), err) : rest

extend :: Environment -> Environment
extend env = (empty, empty):env

arglist_env :: Environment -> LispVal -> LispVal -> Environment
arglist_env env Nil Nil = env
arglist_env _ ks Nil = error $ "Too few arguments: still expecting " ++ show ks
arglist_env _ Nil vs = error $ "Too many arguments: " ++ show vs
arglist_env env (Cell (Sym k) ks) (Cell v vs) = arglist_env (bind env k v) ks vs
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

fromList :: [(String, LispVal)] -> Environment
fromList lst = [(Data.Map.fromList lst, empty)]

lisp_length :: Num a => LispVal -> a
lisp_length (Cell _ rest) = 1 + (lisp_length rest)
lisp_length _ = 0

lisp_error :: String -> LispVal -> LispVal
lisp_error errType val = Cell (Sym "error") (Cell (Sym errType) val)
