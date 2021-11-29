main = print $ take 20 fibs

fibs :: [Int]
fib :: Int -> Int
fibs = map fib [0..]
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))
