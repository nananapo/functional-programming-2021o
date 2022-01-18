
data Complex = Complex Integer Integer

instance Show Complex where
  show (Complex a b) | b < 0 = show a ++ " - " ++ show (abs b) ++ "i"
                    | b > 0 = show a ++ " + " ++ show b ++ "i"
                    | otherwise = show a

instance Num Complex where{
  (Complex a b) + (Complex c d) = Complex (a + c) (b + d);
  (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c);
  negate (Complex a b) = Complex (-a) (-b);
  signum (Complex a b) = Complex (signum a) 0;
  abs (Complex a b) = Complex (abs a) (abs b);
  fromInteger a = Complex (fromInteger a) 0;
}

complexdiv:: Complex -> Complex -> Complex
complexdiv (Complex a b) (Complex c d) = Complex ((a * c + b * d)`div`(c*c+d*d)) ((b * c - a * d)`div`(c*c+d*d))

main ::IO()
main = print $ complexdiv (Complex 5 0) (Complex 1 2)