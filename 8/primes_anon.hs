main = print $ take 100 $ primes [2..]
primes (x:xs) = x : primes((\ = filter (\y=y `mod` x /= 0) xs) x xs)
