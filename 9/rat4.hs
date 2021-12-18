data Rat = Rat Integer Integer

instance Show Rat where 
    show (Rat x y) = show x ++ "/" ++ show y

add::Rat -> Rat -> Rat
add (Rat x y) (Rat u v) = Rat (x * v + u * y) (y * v)

mult::Rat -> Rat -> Rat
mult (Rat x y) (Rat u v) = Rat (x * u) (y * v)

instance Num Rat where
  x + y = add x y
  x * y = mult x y
  negate (Rat x y) = Rat (-x) y
  abs (Rat x y) = Rat (abs x) (abs y)
  signum (Rat x y) | x == 0    = fromInteger 0
                   | x * y > 0 = fromInteger 1
                   | otherwise = fromInteger (-1)
  fromInteger x = Rat x 1

main = print $ Rat 1 2 + Rat 1 6
