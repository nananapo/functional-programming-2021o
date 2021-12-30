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

main = do { cs <- getContents;
putStr $ unlines $ map (unwords . (map show) . tokens) $ lines cs }