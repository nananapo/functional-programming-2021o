main = getContents >>= print . countLine

countLine [] = 0
countLine ('\n':cs) = 1 + countLine(cs)
countLine (_:cs) = countLine(cs)
