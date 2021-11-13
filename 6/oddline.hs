main = getContents >>= putStr . unlines . oddLines . lines

oddLines [] = []
oddLines (c1:c2:c3) = c1:oddLines c3
oddLines (c1:c2) = [c1]
oddLines c1 = c1
