div2:: Int -> Maybe Int
div2 x = if x `mod` 2 == 0 then Just (x `div` 2) else Nothing

div8:: Int -> Maybe Int
div8 a = div2 a >>= (\b -> div2 b) >>= (\c -> div2 c)