myIf :: Bool -> a -> a -> a
myIf True t e = t
myIf False t e = e

f n = myIf (n==5) (1+2) (8 `div` 0)
