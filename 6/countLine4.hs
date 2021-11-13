main = getContents >>= print . countLine
countLine cs = length $ filter (\ch -> ch == '\n') cs
