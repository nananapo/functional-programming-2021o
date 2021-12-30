import Data.Char

data Token = Num Int | Add | Sub | Mul | Div deriving Show

tokens::String -> [Token]
tokens [] = []
tokens ('+':cs) = Add:(tokens cs)
tokens ('-':cs) = Sub:(tokens cs)
tokens ('*':cs) = Mul:(tokens cs)
tokens ('/':cs) = Div:(tokens cs)
tokens (c:cs) | isDigit c = let (ds,rs) = span isDigit (c:cs)
                            in Num(read ds):(tokens rs)

calc::[Token] -> Int
calc [Num x] = x
calc (Num x:Add:Num y:ts) = calc (Num (x+y):ts)
calc (Num x:Sub:Num y:ts) = calc (Num (x-y):ts)
calc (Num x:Mul:Num y:ts) = calc (Num (x*y):ts)
calc (Num x:Div:Num y:ts) = calc (Num (x `div` y):ts)

main = do { cs <- getContents;
putStr $ unlines $ map (show . calc . tokens) $ lines cs }