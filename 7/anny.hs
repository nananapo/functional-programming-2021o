any p [] = False
any p (x:xs) = if p x then True else any p xs

all p x = not $ any (not . p) x
