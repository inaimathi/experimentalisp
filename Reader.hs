module Reader (lisp_read) where

import Model

import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec hiding (spaces)

-- tests :: [String]
-- tests = ["  def "
--         , "123"
--         , "-234"
--         , "-test"
--         , "\"test\""
--         , "(1 2 3)"
--         , "(def null? (fn (thing) (= () thing)))"
--         , "true"
--         , "false"]

lisp_read :: String -> Either ParseError LispVal
lisp_read str = parse lispval "lisp" str

lispval :: Parser LispVal
lispval = do spaces
             exp <- s_expression <|> Reader.string <|> quoted <|> dashed <|> number <|> symbol
             spaces
             return $ exp

s_expression :: Parser LispVal
s_expression = do char '('
                  spaces
                  things <- sepBy lispval spaces
                  spaces
                  char ')'
                  return $ toCells things
                      where toCells [] = Nil
                            toCells (c:rest) = Cell c $ toCells rest

string ::  Parser LispVal
string = do char '"'
            str <- many $ escapes <|> noneOf "\"\\"
            char '"'
            return $ Str str

quoted :: Parser LispVal
quoted = do char '\''
            thing <- lispval
            return $ (Cell (Sym "quote") thing)

dashed :: Parser LispVal
dashed = do char '-'
            str <- many $ noneOf ")\n\r\t "
            return $ if [] == str
                     then Sym $ "-"
                     else if all isNumber str
                          then Num . Prelude.read $ '-':str
                          else Sym $ '-':str

symbol :: Parser LispVal
symbol = do sym <- many1 $ noneOf ")\n\r\t "
            return $ case sym of
                       "true" -> Bool True
                       "false" -> Bool False 
                       _ -> Sym sym

number :: Parser LispVal
number = liftM (Num . Prelude.read) $ many1 digit

spaces :: Parser ()
spaces = skipMany space

escapes :: Parser Char
escapes = do char '\\' 
             x <- oneOf "\\\"nrt" 
             return $ case x of 
                        '\\' -> '\\'
                        '"'  -> '"'
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'
                        c    -> c
