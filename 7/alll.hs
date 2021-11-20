alll p [] = True
alll p (x:xs) = if p x then alll p xs else False

anny p xs = not alll (not . p) xs
