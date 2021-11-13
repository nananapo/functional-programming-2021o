main = getContents >>= putStr . firstNLines 10
firstNLines = unlines . take . lines
