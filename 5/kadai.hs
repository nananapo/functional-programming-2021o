import Data.Char

caesar s = map enc s
  where enc c = case (ord c) of {
    m | (64 < m && m < 90) || (96 < m && m < 122) -> chr (m + 1)
      | m == 90  -> 'A'
      | m == 122 -> 'a'
  }
