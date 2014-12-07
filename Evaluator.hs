module Evaluator where

import Model

eval :: LispVal -> Environment -> LispVal
eval (Sym exp) env = Model.lookup env exp
eval (Cell (Sym "lambda") (Cell args body)) env = 
    Procedure env args body
eval (Cell (Sym "fexpr") (Cell args body)) env =
    Procedure env args body
eval (Cell (Sym "do") exp) env = 
    eval_sequence exp env
eval (Cell (Sym "if") (Cell test (Cell conseq (Cell alt Nil)))) env = 
    if true_p $ eval test env 
    then eval conseq env
    else eval alt env
eval (Cell (Sym "def") (Cell name (Cell exp Nil))) env = 
    eval_definition name exp env
eval (Cell (Sym "set!") (Cell name (Cell exp Nil))) env =
    eval_assignment name exp env
eval (Cell car cdr) env = 
    apply car cdr env
eval exp _ 
    | self_evaluating_p exp = exp
    | otherwise = error $ "EVAL :: invalid form " ++ show exp

eval_sequence :: LispVal -> Environment -> LispVal
eval_sequence (Cell car Nil) env = eval car env
eval_sequence (Cell car cdr) env = eval_sequence cdr env

eval_definition :: LispVal -> LispVal -> Environment -> LispVal
eval_definition (Sym name) exp env = case Model.lookup env name of
                                       Nil -> eval exp env -- bind env name $ eval exp env
                                       _ -> error $ "Symbol '" ++ name ++ "' already bound ..."

eval_assignment :: LispVal -> LispVal -> Environment -> LispVal
eval_assignment (Sym name) exp env = case Model.lookup env name of
                                       Nil -> eval exp env -- set env name $ eval exp env
                                       _ -> error $ "Tried to assign to unbound symbol '" ++ name ++ "' ..."

apply :: LispVal -> LispVal -> Environment -> LispVal
apply exp args env = case eval exp env of
                      Primitive fn arglist -> 
                          fn $ extend_env env arglist values
                      Procedure local_env arglist body -> 
                          eval_sequence body $ extend_env local_env arglist values
                      Fexpr local_env arglist body -> 
                          eval (eval_sequence body $ extend_env local_env arglist args) env
    where values = traverse (flip eval env) args
