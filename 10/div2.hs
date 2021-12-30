div2:: Int -> Maybe Int
div2 x = if x `mod` 2 == 0 then Just (x `div` 2) else Nothing