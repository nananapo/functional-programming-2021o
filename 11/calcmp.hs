-- monad parser –
import Control.Applicative
import Data.Char

data Parser a = Parser (String -> Maybe (a, String))

parse::Parser a -> String -> Maybe (a, String)
parse (Parser p) cs = p cs

instance Functor Parser where {
  fmap f p =
    Parser (\cs -> do { (v, cs1) <- parse p cs ;
                        return (f v, cs1) })
}

instance Applicative Parser where {
  pure v = Parser (\cs -> return (v, cs)) ;
  p <*> q =
    Parser (\cs -> do { (f, cs1) <- parse p cs ;
                        (v, cs2) <- parse q cs1 ;
                        return (f v, cs2) })
}

instance Monad Parser where {
  p >>= f =
    Parser (\cs -> do { (v, cs1) <- parse p cs ;
                        parse (f v) cs1 }) ;
  return x = Parser (\cs -> return (x, cs))
}

instance Alternative Parser where {
  empty = Parser (\cs -> Nothing) ;
  p <|> q = Parser (\cs -> parse p cs <|> parse q cs)
}

-- parser for calculator –

parseOne::Parser Char
parseOne = Parser p where {
  p [] = Nothing ;
  p (c:cs) = Just (c, cs)
}

parseSat::(Char -> Bool) -> Parser Char
parseSat f = do { x <- parseOne ;
                  if f x then return x else empty }

parseChar::Char -> Parser Char
parseChar x = parseSat (== x)

parseString::String -> Parser String
parseString [] = return []
parseString (x:xs) = do { parseChar x ;
                          parseString xs ;
                          return (x:xs) }

parseSpace::Parser ()
parseSpace = do { many (parseSat isSpace) ;
                  return () }

parseNumber::Parser Integer
parseNumber = do { parseSpace ;
                   cs <- some (parseSat isDigit) ;
                   return (read cs) }

parseSymbol::String -> Parser String
parseSymbol xs = do { parseSpace ;
                      parseString xs }

parseFactor::Parser Integer
parseFactor = parseNumber
              <|>
              do { parseSymbol "(" ;
                   x <- parseExpr ;
                   parseSymbol ")" ;
                   return x }

parseTerm::Parser Integer
parseTerm = parseFactor >>= nextFactor where {
  nextFactor x = do { parseSymbol "*" ;
                      y <- parseFactor ;
                      nextFactor (x * y) }
                 <|>
                 do { parseSymbol "/" ;
                      y <- parseFactor ;
                      nextFactor (x `div` y) }
                 <|>
                 return x
}

parseExpr::Parser Integer
parseExpr = parseTerm >>= nextTerm where {
  nextTerm x = do { parseSymbol "+" ;
                    y <- parseTerm ;
                    nextTerm (x + y) }
               <|>
               do { parseSymbol "-" ;
                    y <- parseTerm ;
                    nextTerm (x - y) }
               <|>
               return x
}

-- main --

main::IO ()
main = getContents >>=
         putStr . unlines . map process . lines where {
  process cs = showResult $ parse parseExpr cs ;
  showResult (Just (x,[])) = "result = " ++ show x ;
  showResult _ = "error: syntax"
}
