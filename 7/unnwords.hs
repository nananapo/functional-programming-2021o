unnwords :: [String] -> String
unnwords [] = ""
unnwords [x] = x
unnwords (x:xs) = x ++ " " ++ unnwords xs
