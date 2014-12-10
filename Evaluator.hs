module Evaluator where

import Model

eval :: LispVal -> Environment -> (LispVal, Environment)
eval (Sym exp) env = (Model.lookup env exp, env)
eval (Cell (Sym "fn") (Cell args body)) env = 
    (Procedure env args body, env)
eval (Cell (Sym "fexpr") (Cell args body)) env =
    (Fexpr env args body, env)
eval (Cell (Sym "do") exp) env = 
    eval_sequence exp env
eval (Cell (Sym "if") (Cell test (Cell conseq (Cell alt Nil)))) env = 
    if true_p . fst $ eval test env 
    then eval conseq env
    else eval alt env
eval (Cell (Sym "def") (Cell name (Cell exp Nil))) env = 
    eval_definition name exp env
eval (Cell (Sym "set!") (Cell name (Cell exp Nil))) env =
    eval_assignment name exp env
eval (Cell (Sym "quote") thing) env =
    (thing, env)
eval (Cell car cdr) env = 
    apply car cdr env
eval exp env 
    | self_evaluating_p exp = (exp, env)
    | otherwise = error $ "EVAL :: invalid form " ++ show exp

eval_sequence :: LispVal -> Environment -> (LispVal, Environment)
eval_sequence (Cell car Nil) env = eval car env
eval_sequence (Cell car cdr) env = eval_sequence cdr new_env
    where (_, new_env) = eval car env
eval_sequence exp _ = error $ "Called eval_sequence on a non-cell: " ++ show exp

eval_args :: LispVal -> Environment -> LispVal
eval_args (Cell car cdr) env = (Cell res (eval_args cdr env))
    where (res, _) = eval car env
eval_args Nil _ = Nil
eval_args argl _ = error $ "Called eval_args on a non-cell: " ++ show argl

eval_definition :: LispVal -> LispVal -> Environment -> (LispVal, Environment)
eval_definition (Sym name) exp env = case Model.lookup env name of
                                       Nil -> (Nil, bind new_env name res)
                                           where (res, new_env) = eval exp env
                                       _ -> error $ "Symbol '" ++ name ++ "' already bound ..."
eval_definition name _ _ = error $ "Tried to bind to non-symbol: " ++ show name

eval_assignment :: LispVal -> LispVal -> Environment -> (LispVal, Environment)
eval_assignment (Sym name) exp env = case Model.lookup env name of
                                       Nil -> (res, bind new_env name res)
                                           where (res, new_env) = eval exp env
                                       _ -> error $ "Tried to assign to unbound symbol '" ++ name ++ "' ..."
eval_assignment name _ _ = error $ "Tried to assign to non-symbol: " ++ show name

apply :: LispVal -> LispVal -> Environment -> (LispVal, Environment)
apply exp args env = 
    case eval exp env of
      (Primitive fn arglist, env') -> 
          fn $ arglist_env (extend env') arglist $ eval_args args env'
      (Procedure local_env arglist body, env') -> 
          eval_sequence body $ arglist_env (extend local_env) arglist $ eval_args args env'
      (Fexpr local_env arglist body, env') -> 
          eval res env'
                where (res, _) = eval_sequence body $ arglist_env (extend local_env) arglist args
      _ -> error $ "Undefined function '" ++ show exp ++ "'"
