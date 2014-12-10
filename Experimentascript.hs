module Main where

import Model
import Reader
import Evaluator

import Haste

setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

prependContent :: ElemID -> String -> IO ()
prependContent id newContent = withElem id (\e -> do cont <- getProp e "innerHTML"
                                                     setProp e "innerHTML" $ concat [newContent, "\n\n", cont]
                                                     return ())

escape :: String -> String
escape [] = []
escape ('<':rest) = "&lt;" ++ escape rest
escape ('>':rest) = "&gt;" ++ escape rest
escape (c:rest) = c : (escape rest)

mk_eval :: Elem -> Environment -> t -> t1 -> IO Environment
mk_eval inp env _ _ = do Just val <- getValue inp
                         case lisp_read val of
                           Right exp -> let (res, env') = Evaluator.eval exp env
                                        in do prependContent "repl-log" . escape $ concat [val, "\n   => ", show res]
                                              return env'                               
                           _ -> do prependContent "repl-log" "Read error ..."
                                   return env

main :: IO ()
main = withElems ["eval-button", "repl-input"] $ \[btn, inp] -> 
       do onEvent btn OnClick $ \_ _ ->
              do Just val <- getValue inp
                 case lisp_read val of
                   Right res -> let (evaled, _) = Evaluator.eval res global_env
                                in prependContent "repl-log" . escape $ concat [val, "\n    => ", show evaled]
                   _ -> writeLog "READ ERROR"
          return ()
