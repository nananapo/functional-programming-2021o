import System.Environment

--main = getArgs >>= \args -> putStrLn $ unwords args

main = getArgs >>= putStrLn . unwords
