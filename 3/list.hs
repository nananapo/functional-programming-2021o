head2 xs = head(tail xs)
leength xs = if null xs then 0 else 1 + leength(tail xs)
suum xs = if null xs then 0 else (head xs) + suum(tail xs)
append xs ys = if null xs then ys else (head xs) : (append (tail xs) ys)
rev xs = if null xs then [] else append (rev(tail xs)) ([head xs])
genodd n = if n==1 then [1] else append (genodd(n-2)) [n]
sl xs = if null xs then [] else ((head xs) * (head xs)) : (sl(tail xs))

os n = if n == 1 then [1] else append (os(n-2)) ([n*n]) 
os2 n = sl(genodd n)