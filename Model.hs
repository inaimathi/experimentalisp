module Model ( LispVal(..)
             , true_p, self_evaluating_p, lisp_prim, lisp_length, lisp_type
             , Environment
             , extend, bind, bind_handler, Model.lookup, lookup_handler
             , Model.fromList, Data.Map.toList) where

import Data.Map hiding (map)

data LispVal = Cell LispVal LispVal
             | Str String
             | Sym String
             | Num Integer
             | Bool Bool
             | Chr Char 
             | Nil
             | Err String LispVal
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
lisp_type (Err _ _) = Type "error"
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
    show (Err t v) = concat ["<ERROR: ", t, " ", show v, ">"] 
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
type Frame = (Tbl, Tbl)
type Environment = [Frame]

extend :: Environment -> Environment
extend env = (empty, empty):env

bind :: Environment -> String -> LispVal -> Environment
bind [] _ _ = []
bind ((env, err):rest) k v = ((insert k v env), err) : rest

bind_handler :: Environment -> String -> LispVal -> Environment
bind_handler [] _ _ = []
bind_handler ((env, err):rest) k v = (env, (insert k v err)) : rest

envLookup :: (Frame -> Tbl) -> Environment -> String -> LispVal
envLookup _ [] _ = Nil
envLookup fn (frame:rest) k  = case Data.Map.lookup k $ fn frame of
                                 Nothing -> envLookup fn rest k
                                 Just v -> v

lookup :: Environment -> String -> LispVal
lookup = envLookup fst

lookup_handler :: Environment -> String -> LispVal
lookup_handler env t = case envLookup snd env t of
                         Nil -> envLookup snd env "*"
                         v -> v

---------- Basic, non-erroring primitives
true_p :: LispVal -> Bool
true_p (Bool False) = False
true_p _ = True

self_evaluating_p :: LispVal -> Bool
self_evaluating_p (Bool _) = True
self_evaluating_p Nil = True
self_evaluating_p (Str _) = True
self_evaluating_p (Num _) = True
self_evaluating_p (Chr _) = True
self_evaluating_p (Err _ _) = True
self_evaluating_p (Primitive _ _) = True
self_evaluating_p (Procedure _ _ _) = True
self_evaluating_p (Fexpr _ _ _) = True
self_evaluating_p _ = False

fromList :: [(String, LispVal)] -> [(String, LispVal)] -> Environment
fromList frame err = [(Data.Map.fromList frame, Data.Map.fromList err)]

lisp_length :: Num a => LispVal -> a
lisp_length (Cell _ rest) = 1 + (lisp_length rest)
lisp_length _ = 0
