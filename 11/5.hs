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

parseSymbol:: String -> Parser String
parseSymbol s = do {    parseSpace;
                        parseString s;}