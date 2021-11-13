main = getContents >>= print . countLine
countLine cs = length $ filter (== '\n') cs
