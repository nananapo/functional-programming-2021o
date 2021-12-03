main = print $ take 20 $ twin $ primes [2..]

sieve x xs  = filter notdiv xs
  where notdiv y = y `mod` x /= 0

primes (x:xs) = x : primes(sieve x xs)

twin :: [Int] -> [(Int,Int)]
twin (a:ls) = if ((head ls) - a) == 2 then (a,head ls) : (twin ls) else twin ls
