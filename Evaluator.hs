module Evaluator ( Result(..), res_of
                 , eval, apply)where

import Model

data Result = Res LispVal
            | Mod LispVal Environment

res_of :: Result -> LispVal
res_of (Res v) = v
res_of (Mod v _) = v

eval :: LispVal -> Environment -> Result
eval (Sym exp) env = 
    Res $ Model.lookup env exp
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
    Res $ apply car cdr env
eval exp _ 
    | self_evaluating_p exp = Res exp
    | otherwise = error $ "EVAL :: invalid form " ++ show exp

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
                                       _ -> error $ "Symbol '" ++ name ++ "' already bound ..."
eval_definition name _ _ = error $ "Tried to bind to non-symbol: " ++ show name

eval_assignment :: LispVal -> LispVal -> Environment -> Result
eval_assignment (Sym name) exp env = case Model.lookup env name of
                                       Nil -> Mod res $ bind new_env name res
                                           where evaled = eval exp env
                                                 res = res_of evaled
                                                 new_env = case evaled of
                                                             Mod _ e -> e
                                                             _ -> env
                                                 
                                       _ -> error $ "Tried to assign to unbound symbol '" ++ name ++ "' ..."
eval_assignment name _ _ = error $ "Tried to assign to non-symbol: " ++ show name

apply :: LispVal -> LispVal -> Environment -> LispVal
apply exp args env = 
    case res_of $ eval exp env of
      Primitive fn arglist -> 
          fn $ arglist_env (extend env) arglist $ eval_args args env
      Procedure local_env arglist body -> 
          res_of $ eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env
      Fexpr local_env arglist body -> 
          res_of $ eval res env
              where res = res_of $ eval_sequence body $ arglist_env (extend local_env) arglist args
      _ -> error $ "Undefined function '" ++ show exp ++ "'"
