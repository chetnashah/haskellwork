


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


sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where
    mark :: [Integer] -> Integer -> Integer -> [Integer]
    mark (y : ys) k m | k == m = 0 : (mark ys 1 m)
                  | otherwise = y : (mark ys (k+1) m)

-- emits a stream with all multiples of m's replaced by zero
mark :: [Integer] -> Integer -> Integer -> [Integer]
mark (y : ys) k m | k == m = 0 : (mark ys 1 m)
                  | otherwise = y : (mark ys (k+1) m)

-- try checking with take 20 $ mark [2..] 2 3
-- try checking with take 20 $ mark [2..] 2 5

-- a mersenne prime is a prime which is one less than a power of 2.
-- in other words it has a form 2^n - 1

primes = sieve [2..]

mersenne = [(p, 2^p - 1) | p <- primes, prime0 (2^p - 1)]


