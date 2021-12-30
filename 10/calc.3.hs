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
                 Time   ParseTree ParseTree |
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
        nextTerm(p1, Add:ts1) = let (p2, ts2) = parseTerm ts1
                                in nextTerm(Plus p1 p2, ts2);
        nextTerm(p1, Sub:ts1) = let (p2, ts2) = parseTerm ts1
                                in nextTerm(Minus p1 p2, ts2);
        nextTerm x = x;
    }

main = do { cs <- getContents;
            putStr $ unlines $ map (show . fst . parseExpr . tokens) $ lines cs }