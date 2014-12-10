module Main where

import Model
import Reader
import Evaluator

import Haste
import Haste.Concurrent

setContent :: ElemID -> String -> IO ()
setContent id newContent = withElem id (\e -> setProp e "innerHTML" newContent)

prependContent :: ElemID -> String -> IO ()
prependContent id newContent = withElem id (\e -> do cont <- getProp e "innerHTML"
                                                     setProp e "innerHTML" $ concat [newContent, "\n\n", cont]
                                                     return ())

escape [] = []
escape ('<':rest) = "&lt;" ++ escape rest
escape ('>':rest) = "&gt;" ++ escape rest
escape (c:rest) = c : (escape rest)

main :: IO ()
main = withElems ["eval-button", "repl-input"] $ \[btn, inp] -> 
       do onEvent btn OnClick $ \_ _ ->
              do Just val <- getValue inp
                 case lisp_read val of
                   Right res -> let (evaled, env') = Evaluator.eval res global_env
                                in prependContent "repl-log" . escape $ concat [val, "\n    => ", show evaled]
                   _ -> writeLog "READ ERROR"
          return ()
