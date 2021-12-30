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

data ParseTree = Number Int |
                 Plus   ParseTree ParseTree |
                 Minus  ParseTree ParseTree |
                 Times  ParseTree ParseTree |
                 Divide ParseTree ParseTree  deriving Show

type Parser = [Token] -> (ParseTree, [Token])

parseNumber::Parser
parseNumber(Num x:ts) = (Number x, ts)

parseTerm::Parser
parseTerm ts = nextNumber $ parseNumber ts
  where { nextNumber x = x }

parseExpr::Parser
parseExpr ts = nextTerm $ parseTerm ts
    where { 
        nextTerm (p1, Add:ts1) = let (p2, ts2) = parseTerm ts1
                                in (Plus p1 (fst $ nextTerm (p2, ts2)),[]);
        nextTerm (p1, Sub:ts1) = let (p2, ts2) = parseTerm ts1
                                in (Minus p1 (fst $ nextTerm (p2, ts2)),[]);
        nextTerm (p1, Mul:ts1) = let (p2, ts2) = parseTerm ts1
                                in nextTerm (Times p1 p2, ts2);
        nextTerm (p1, Div:ts1) = let (p2, ts2) = parseTerm ts1
                                in nextTerm (Divide p1 p2, ts2);
        nextTerm x = x;
    }

eval::ParseTree -> Int
eval (Number x) = x
eval (Plus   p1 p2) = eval p1 + eval p2
eval (Minus  p1 p2) = eval p1 - eval p2
eval (Times  p1 p2) = eval p1 * eval p2
eval (Divide p1 p2) = eval p1 `div` eval p2

main = do { cs <- getContents;
            putStr $ unlines $ map (show  . fst . parseExpr . tokens) $ lines cs }