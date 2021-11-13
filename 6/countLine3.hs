main = getContents >>= print . countLine

countLine cs = length $ filter eqln cs
  where eqln ch = ch == '\n'
