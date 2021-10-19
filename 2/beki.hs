beki x y = if y == 0 then 1 else x * beki x (y-1)
beki2 x y = if y == 0 then 1 else if y `mod` 2 == 0 then beki2 (x*x) (y `div` 2) else x * (beki2 x (y-1))