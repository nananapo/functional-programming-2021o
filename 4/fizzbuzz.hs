fizzBuzz x = map toFBText [1..x] where{ 
  isFizz x = x `mod` 3 == 0;
  isBuzz x = x `mod` 5 == 0;
  isFizzBuzz x = (isFizz x) && (isBuzz x);
  toFBText x = if isFizzBuzz x then "FizzBuzz" else if isFizz x then "Fizz" else if isBuzz x then "Buzz" else show x
}
