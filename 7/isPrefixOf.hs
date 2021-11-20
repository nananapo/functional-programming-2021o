isPreefixOf [] ys = True
isPreefixOf xs [] = False
isPreefixOf (x:xs) (y:ys) =if x == y then isPreefixOf xs ys else False
