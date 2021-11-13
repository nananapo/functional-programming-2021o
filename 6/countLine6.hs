main = getContents >>= print . length . filter (== '\n')
