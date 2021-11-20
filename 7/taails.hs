taails [] = [[]]
taails (x:xs) = [x:xs] ++ taails xs
