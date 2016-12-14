

-- whether n can be divided into d parts , n > d
-- mathematical notation is d | n which can also be said as
-- whether d is a divisor of n. Defn of divisor : Wikipedia
divides d n = n `rem` d == 0

-- least divisor of n
ld n = ldf 2 n

-- Theorem : ld n is always prime

-- least divisor of n starting from k
ldf k n
  | divides k n = k
  | k ^ 2 > n = n
  | otherwise = ldf (k+1) n

-- Theorem: a no greater than 1 is prime if ld n == n
prime0 n
  | n < 0     = error "not a positive integer"
  | n == 1    = False
  | otherwise = ld n == n

  
