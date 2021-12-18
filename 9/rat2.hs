data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat x y) = show x ++ "/" ++ show y

main = print $ Rat 2 3
