module Main where

import Model
import Reader
import Evaluator

import Haste
import Data.IORef

prependContent :: ElemID -> String -> IO ()
prependContent id newContent = 
    withElem id (\e -> do cont <- getProp e "innerHTML"
                          setProp e "innerHTML" $ concat [newContent, "\n\n", cont]
                          return ())

escape :: String -> String
escape [] = []
escape ('<':rest) = "&lt;" ++ escape rest
escape ('>':rest) = "&gt;" ++ escape rest
escape (c:rest) = c : (escape rest)

eval_contents :: Elem -> Environment -> IO Environment
eval_contents inp env = 
    do Just val <- getValue inp
       case lisp_read val of
         Right exp -> let (res, env') = Evaluator.eval exp env
                      in do prependContent "repl-log" . escape $ concat [val, "\n   => ", show res]
                            return env'                               
         _ -> do prependContent "repl-log" "Read error ..."
                 return env

main :: IO ()
main = withElem "repl-input" $ \inp -> 
       do env_ref <- newIORef global_env 
          onEvent inp OnKeyPress $ \k -> 
              case k of
                13 -> do env <- readIORef env_ref 
                         env' <- eval_contents inp env
                         _ <- writeIORef env_ref env'
                         return ()
                _ -> return ()
          return ()
