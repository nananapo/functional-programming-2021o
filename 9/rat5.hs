import System.Environment

data Rat = Rat Integer Integer

maxdivisor::Integer -> Integer -> Integer
maxdivisor x y = let 
                   m = min (abs x) (abs y) 
                 in 
                   1 + m - (head $ filter (\n -> (x `mod` (m-n+1)) == 0 && (y `mod` (m-n+1)) == 0) [1..m])

add::Rat -> Rat -> Rat
add (Rat x y) (Rat u v) = Rat (x * v + u * y) (y * v)

mult::Rat -> Rat -> Rat
mult (Rat x y) (Rat u v) = Rat (x * u) (y * v)

reduction::Rat -> Rat
reduction (Rat x y) | x == 0    = Rat 0 1
                    | otherwise = let 
                                    m = maxdivisor x y 
                                  in 
                                    if m == 1 then Rat x y else reduction (Rat (x `div` m) (y `div` m))

normalize::Rat -> Rat
normalize (Rat x y) | x == 0                 = Rat 0 1
                    | x < 0 && y < 0         = Rat (-x) (-y)
                    | (x * y < 0) && (y < 0) = Rat (-x) (-y)
                    | otherwise              = Rat x y

instance Show Rat where
    show r = let 
               (Rat x y) = reduction $ normalize $ r
             in
               if x == 0 then show 0
               else if y == 1 then show x 
               else show x ++ "/" ++ show y
    
instance Num Rat where
  x + y = normalize $ reduction $ add x y
  x * y = normalize $ reduction $ mult x y
  negate (Rat x y) = Rat (-x) y
  abs (Rat x y) = Rat (abs x) (abs y)
  signum (Rat x y) | x == 0    = fromInteger 0
                   | x * y > 0 = fromInteger 1
                   | otherwise = fromInteger (-1)
  fromInteger x = Rat x 1
    
main :: IO ()
main = getArgs >>= \args -> test $ map read args
  where test (x:y:u:v:_) = (print $ Rat x y + Rat u v) >>
                           (print $ Rat x y - Rat u v) >>
                           (print $ Rat x y * Rat u v)
