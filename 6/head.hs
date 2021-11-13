main = getContents >>= putStr . firstNLines 10
firstNLines n cs = unlines $ take n $ lines cs
