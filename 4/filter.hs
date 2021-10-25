fiilter f xs = if null xs then [] else if f (head xs) then (head xs):fiilter f (tail xs) else fiilter f (tail xs)

