
-- this is a shitty solution, took almost a minute
divides y = \x -> (y `mod` x == 0)  

-- whether n is divisible by all numbers from 1..k
dividableByAll :: Int -> Int -> Bool
dividableByAll k n = all (divides n) [1..k]

divideableBy20 = dividableByAll 20

findFirstDivisibleByAll = take 1 $ dropWhile (\x -> ((divideableBy20 x) == False)) [1..100000000000]  

main = do
  putStrLn $ " dividable by all" ++ show (findFirstDivisibleByAll)


-- simplest solution
-- foldr1 does not need a zer0/unit, in foldr1 the unit is 1
-- foldr1 only needs combiner and array
-- foldr1 lcm [1..20]

-- or in simple terms
-- foldr lcm 1 [1..20]
