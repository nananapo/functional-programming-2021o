fact x = if x == 0 then 1 else x * fact(x)
comb n m = fact(n) / fact(m) / fact(n-m)
comb2 n m = if n == m || n == 0 then 1 else comb2 (n-1) (m-1) + comb2 (n-1) m