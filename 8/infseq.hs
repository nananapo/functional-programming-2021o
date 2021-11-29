main = print $ infseq 100

infseq :: Int -> [Int]
infseq n = take n $ inf 1
    where inf n = n:(0-n):inf (n+1)
