primes n = filter isPrime [1..n]
  where {
isPrime x = length(divisors x) == 2;
divisors x = filter divisible [1..x]
where{
divisible y = x `mod` y == 0
}
  }  
