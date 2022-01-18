import Data.Char
import Control.Applicative

data Parser a = Parser (String -> Maybe (a,String))

instance Functor Parser where
    fmap f p = Parser (\s -> do {
        (v,cs1) <- parse p s;
        return (f v, cs1)
    })

instance Applicative Parser where
    pure v = Parser (\cs -> return (v,cs))
    p <*> q = Parser (\cs -> do {   (f,cs1) <- parse p cs; 
                                    (v,cs2) <- parse q cs1; 
                                    return (f v,cs2)})
instance Monad Parser where
    p >>= f = Parser (\cs -> do {   (v,cs1) <- parse p cs;
                                    parse (f v) cs1;})
    return x = Parser (\cs -> return (x,cs))

instance Alternative Parser where
    empty = Parser (\cs -> Nothing)
    p <|> q = Parser (\cs -> parse p cs <|> parse q cs)

parse:: Parser a -> String -> Maybe (a,String)
parse (Parser p) cs = p cs

parseOne::Parser Char
parseOne = Parser p where{
    p [] = Nothing;
    p (c:cs) = Just (c,cs);}

parseSat::(Char -> Bool) -> Parser Char
parseSat f = do {   x <- parseOne;
                    if f x then return x else empty}

parseChar:: Char -> Parser Char
parseChar c = parseSat (==c)

parseString:: String -> Parser String
parseString [] = return []
parseString (x:xs) = do {   parseChar x;
                            parseString xs;
                            return (x:xs)}

parseSpace:: Parser ()
parseSpace = do {   many (parseSat isSpace);
                    return ()}
parseNumber:: Parser Integer
parseNumber = do {  parseSpace;
                    n <- some (parseSat isDigit);
                    return (read n)}

parseComplex:: Parser Complex
parseComplex =  do {parseSymbol "i";
                    return (Complex 0 1)}
                <|> 
                do {n <- parseNumber;
                    parseSymbol "i";
                    return (Complex 0 n)}
                <|>
                do {n <- parseNumber;
                    return (Complex n 0)}

parseSymbol:: String -> Parser String
parseSymbol s = do {    parseSpace;
                        parseString s;}

parseFactor:: Parser Complex
parseFactor =   parseComplex 
                <|> 
                do {    parseSymbol "(";
                        n <- parseExpr;
                        parseSymbol ")";
                        return n}
                <|>
                do {    parseSymbol "-";
                        n <- parseFactor;
                        return (-n)}
                <|>
                do {    parseSymbol "+";
                        n <- parseFactor;
                        return n}

parseTerm:: Parser Complex
parseTerm = parseFactor >>= nextFactor where{
    nextFactor x =  do{ parseSymbol "*";
                        y <- parseFactor;
                        nextFactor (x*y);}
                    <|> 
                    do{ parseSymbol "/";
                        y <- parseFactor;
                        nextFactor (complexdiv x y);}
                    <|>
                    return x}

parseExpr:: Parser Complex
parseExpr = parseTerm >>= nextTerm where {
    nextTerm x = do {   parseSymbol "+";
                        y <- parseTerm;
                        nextTerm (x+y);}
                    <|>
                    do{ parseSymbol "-";
                        y <- parseTerm;
                        nextTerm (x-y);}
                    <|>
                    return x}

main::IO()
main = getContents >>= putStr . unlines . map process . lines where{
    process cs = showResult $ parse parseExpr cs;
    showResult (Just (x,[])) = "result = " ++ show x;
    showResult _ = "error: syntax";}


data Complex = Complex Integer Integer

instance Show Complex where
  show (Complex a b) | b < 0 = show a ++ " - " ++ show (abs b) ++ "i"
                    | b > 0 = show a ++ " + " ++ show b ++ "i"
                    | otherwise = show a

instance Num Complex where{
  (Complex a b) + (Complex c d) = Complex (a + c) (b + d);
  (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c);
  negate (Complex a b) = Complex (-a) (-b);
  signum (Complex a b) = Complex (signum a) 0;
  abs (Complex a b) = Complex (abs a) (abs b);
  fromInteger a = Complex (fromInteger a) 0;
}

complexdiv:: Complex -> Complex -> Complex
complexdiv (Complex a b) (Complex c d) = Complex ((a * c + b * d)`div`(c*c+d*d)) ((b * c - a * d)`div`(c*c+d*d))