fizzbuzz :: Int -> [String];
fizzbuzz x = map fb [1..x] where{
  fizz :: Int -> String;
  buzz :: Int -> String -> String;
  number :: Int -> String -> String;
  fb :: Int -> String;

  fizz x = if (x `mod` 3) == 0 then "Fizz" else "";
  buzz x s = if (x `mod` 5) == 0 then s ++ "Buzz" else s;
  number x s = if null s then show x else s;
  fb x = number x (buzz x (fizz x));
}

ziip :: [a] -> [b] -> [(a,b)]
ziip xs ys = 
  if (null xs) || (null ys) 
    then []
    else (head xs,head ys):(zip (tail xs) (tail ys)) 

unziip :: [(a,b)] -> ([a],[b])
unziip t = 
  if null t
    then ([],[])
    else (fst (head t):fst (unziip (tail t)),snd (head t):snd (unziip (tail t)))

unziiip :: [(a,b)] -> ([a],[b])
unziiip t = (map fst t, map snd t)

divisors :: Int -> [Int]
divisors x = filter divisible [1..x] 
  where divisible y = x `mod` y == 0

divisors2 :: Int -> [Int]
divisors2 x = let divisible y = x `mod` y == 0
  in filter divisible [1..x]

map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

append [] ys = ys
append (x:xs) ys = x:append xs ys

lstrip str@(c:cs) = if isSpace c then lstrip cs else str
  where isSpace s = s == ' '

leapYear year 
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise -> False