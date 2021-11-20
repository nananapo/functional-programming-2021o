import System.Environment
main = getArgs >>= \args -> print $ sum $ map read args
