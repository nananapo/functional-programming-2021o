import Control.Applicative

data Parser a = Parser (String -> Maybe (a,String))

instance Functor Parser where
    fmap f p = Parser $ \s -> do {
        (v,cs1) <- parse p s;
        return (f v, cs1)
    }

parse:: Parser a -> String -> Maybe (a,String)
parse (Parser p) cs = p cs

parseOne::Parser Char
parseOne = Parser p where{
    p [] = Nothing;
    p (c:cs) = Just (c,cs);
}