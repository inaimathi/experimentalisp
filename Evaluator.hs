module Evaluator ( Result(..), res_of
                 , eval, apply, mk_type
                 , global_env) where
    
import Model
import Reader

data Result = Res LispVal
            | Mod LispVal Environment deriving Show

arglist_env :: Environment -> LispVal -> LispVal -> Environment
arglist_env env Nil Nil = env
arglist_env _ ks Nil = error $ "Too few arguments: still expecting " ++ show ks
arglist_env _ Nil vs = error $ "Too many arguments: " ++ show vs
arglist_env env (Cell (Sym k) ks) (Cell v vs) = arglist_env (bind env k v) ks vs
arglist_env env ks vs = error . unlines $ ["Something odd happened", show ks, show vs, show env]

mk_type :: LispVal -> LispVal
mk_type (Sym v) = Type v
mk_type (Cell (Sym v) Nil) = (Cell (Type v) Nil)
mk_type (Cell a b) = (Cell (mk_type a) (mk_type b))
mk_type Nil = Nil
mk_type val = error "invalid type"

lisp_error :: String -> LispVal -> Environment -> Result
lisp_error errType val env = eval (Cell (Sym "raise") (Cell (Err errType val) Nil)) env

res_of :: Result -> LispVal
res_of (Res v) = v
res_of (Mod v _) = v

eval :: LispVal -> Environment -> Result
eval (Sym exp) env = 
    Res $ Model.lookup env exp
eval (Cell (Sym "error") (Cell (Sym errT) (Cell val Nil))) env =
    Res $ Err errT val
eval (Cell (Sym "handler") (Cell (Sym errT) (Cell handler Nil))) env =
    Mod Nil $ bind_handler env errT handler
eval (Cell (Sym "list") rest) env =
    Res $ eval_args rest env
eval (Cell (Sym "fn") (Cell args body)) env = 
    Res $ Procedure env args body
eval (Cell (Sym "fexpr") (Cell args body)) env =
    Res $ Fexpr env args body
eval (Cell (Sym "do") exp) env = 
    eval_sequence exp env
eval (Cell (Sym "if") (Cell test (Cell conseq (Cell alt Nil)))) env = 
    if true_p . res_of $ eval test env 
    then eval conseq env
    else eval alt env
eval (Cell (Sym "def") (Cell name (Cell exp Nil))) env = 
    eval_definition name exp env
eval (Cell (Sym "set!") (Cell name (Cell exp Nil))) env =
    eval_assignment name exp env
eval (Cell (Sym "quote") thing) _ =
    Res thing
eval (Cell car cdr) env = 
    apply car cdr env
eval err@(Err _ _) env = 
    Res err
eval exp env 
    | self_evaluating_p exp = Res exp
    | otherwise = lisp_error "invalid-form" exp env

eval_sequence :: LispVal -> Environment -> Result
eval_sequence (Cell car Nil) env = eval car env
eval_sequence (Cell car cdr) env = eval_sequence cdr new_env
    where new_env = case eval car env of
                      (Res _) -> env
                      (Mod _ e) -> e
eval_sequence exp _ = error $ "Called eval_sequence on a non-cell: " ++ show exp

eval_args :: LispVal -> Environment -> LispVal
eval_args (Cell car cdr) env = (Cell res (eval_args cdr env))
    where res = res_of $ eval car env
eval_args Nil _ = Nil
eval_args argl _ = error $ "Called eval_args on a non-cell: " ++ show argl

eval_definition :: LispVal -> LispVal -> Environment -> Result
eval_definition (Sym name) exp env = case Model.lookup env name of
                                       Nil -> Mod Nil env''
                                           where evaled = eval exp env
                                                 env' = case evaled of
                                                             (Res _) -> env
                                                             (Mod _ e) -> e
                                                 env'' = bind env' name res
                                                 res = case res_of evaled of
                                                         Procedure _ args body -> Procedure env'' args body
                                                         Fexpr _ args body -> Fexpr env'' args body
                                                         exp -> exp
                                       _ -> lisp_error "invalid-bind" (Sym name) env
eval_definition name _ env = lisp_error "invalid-bind-target" name env

eval_assignment :: LispVal -> LispVal -> Environment -> Result
eval_assignment (Sym name) exp env = case Model.lookup env name of
                                       Nil -> Mod res $ bind new_env name res
                                           where evaled = eval exp env
                                                 res = res_of evaled
                                                 new_env = case evaled of
                                                             Mod _ e -> e
                                                             _ -> env
                                                 
                                       _ -> lisp_error "invalid-assignment" (Sym name) env
eval_assignment name _ env = lisp_error "invalid-assignment-target" name env

apply :: LispVal -> LispVal -> Environment -> Result
apply exp args env = 
    case res_of $ eval exp env of
      Primitive fn arglist -> 
          case lisp_length arglist `compare` lisp_length evaled of
            LT -> lisp_error "too-many-arguments" evaled env
            GT -> lisp_error "too-few-arguments" Nil env
            _  -> Res $ fn $ arglist_env (extend env) arglist evaled
          where evaled = eval_args args env
      Procedure local_env arglist body -> 
          eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env
      Fexpr local_env arglist body -> 
          eval res env
              where res = res_of $ eval_sequence body $ arglist_env (extend local_env) arglist args
      _ -> lisp_error "undefined-function" exp env

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


             , ("raise", lisp_prim ["err"] (\env [Err t v] -> lookup_handler env t))

             , ("the-env", lisp_prim [] (\env _ -> lisp_env env))
             , ("read", lisp_prim ["exp"] (\_ [Str exp] -> case lisp_read exp of
                                                              Right val -> val
                                                              _ -> error $ "Invalid expression: '" ++ exp ++ "'..."))
             , ("write", lisp_prim ["exp"] (\_ [exp] -> Str $ show exp))


               
             , ("type", lisp_prim ["v"] (\_ [val] -> mk_type val))
             , ("type-of", lisp_prim ["thing"] (\_ [v] -> lisp_type v))
             -- type-check
             -- 
             ] [("*", lisp_prim ["err"] (\_ [v] -> Str $ show v))]










