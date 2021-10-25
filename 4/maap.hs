maap f xs = if null xs then [] else (f(head xs)):(maap f (tail xs))

