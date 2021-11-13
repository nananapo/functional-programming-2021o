main = getContents >>= print . countLine

countLine cs = length (filter eqln cs) + 1
  where eqln ch = ch == '\n'
