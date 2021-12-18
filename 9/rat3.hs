data Rat = Rat Integer Integer

instance Show Rat where 
    show (Rat x y) = show x ++ "/" ++ show y

add::Rat -> Rat -> Rat
add (Rat x y) (Rat u v) = Rat (x * v + u * y) (y * v)

main = print $ add (Rat 1 2) (Rat 1 6)
