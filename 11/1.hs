data Parser a = Parser (String -> Maybe (a,String))

parse:: Parser a -> String -> Maybe (a,String)
parse (Parser p) cs = p cs

parseOne::Parser Char
parseOne = Parser p where{
    p [] = Nothing;
    p (c:cs) = Just (c,cs);
}