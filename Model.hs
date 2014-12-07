module Model (LispVal(..))where

data LispVal = Cell LispVal LispVal
             | Str String
             | Sym String
             | Num Integer
             | Bool Bool
             | Chr Char 
             | Nil
         deriving (Eq, Ord)

instance Show LispVal where
    show (Str s) = show s
    show (Sym s) = s
    show (Num n) = show n
    show cell@(Cell _ _) = concat $ "(" : (recur cell)
        where recur (Cell car Nil) = [show car, ")"]
              recur (Cell car cdr@(Cell _ _)) = show car : " " : (recur cdr)
              recur (Cell car cdr) = [show car, " . ", show cdr]
              recur _ = error "Invalid form in cell recur"
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (Chr c) = "#" ++ show c
    show Nil = "NIL"
