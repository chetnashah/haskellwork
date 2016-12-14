


mnmInt :: [Int] -> Int
mnmInt [] = error "impossible to find min of empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- using inbuild function max, return a maximum of list of ints
-- 1.9 mxmInt
-- base cases first
mxmInt :: [Int] -> Int
mxmInt [] = error "impossible to find max of emptylist"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs) -- we put brackets because fn application associates to left

-- 1.10 removeFst
-- remove first occurence of int m from a list of integers
-- list remains unchanged if m is not present in list
removeFst :: Int -> [Int] -> [Int]
removeFst a [] = []
removeFst a (x:xs)
  | a == x = xs
  | otherwise = [x] ++  removeFst a xs

-- sorting ints using removefirst and mnmInt
srtInts :: [Int] -> [Int]
srtInts [] = [] -- empty list is already sorted
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

-- where and let are brothers
srtInts2 :: [Int] -> [Int]
srtInts2 [] = []
srtInts2 xs = let
  m = mnmInt xs
  in m : (srtInts2 (removeFst m xs))


-- ex 1.13
-- count a given charachter in string
count :: Char -> String -> Int
count _ "" = 0
count c (x:xs)
  | c == x = 1 + count c xs
  | otherwise = count c xs

-- ex 1.14
-- tranform string to repeating string
blowup :: String -> String
blowup "" = ""
blowup xs = blowuprev (reverse xs) where blowuprev (x:xs) = blowuprev xs ++ replicate (length xs + 1) x
                                         blowuprev "" = ""


-- find minimum string in a list of strings
mnmString :: [String] -> String
mnmString [] = error "no minimum string in empty list"
mnmString [x] = x
mnmString (x:xs) = min x (mnmString xs) 

-- removefst String
removeFstString :: String -> [String] -> [String]
removeFstString a [] = []
removeFstString a (x:xs)
  | a == x = xs
  | otherwise = [x] ++  removeFstString a xs


-- ex 1.15
-- write a function srtString that sorts a list of strings in
-- lexicographic order (Strings have < which is a proper lexicographic compare
srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : srtString (removeFstString m xs) where m = mnmString xs

-- prefex properties
-- 1. empty arr is prefix of any array
-- 2. if xs is prefix of ys, then so is x:xs of x:ys
prefix :: String -> String -> Bool
prefix [] xs = True
prefix (x:xs) [] = False -- if there is element in left it cannot be prefix of empty
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- ex 1.17 (my solution)
-- returns whether str1 is a substring of str2
subString :: String -> String -> Bool
subString "" ys = True
subString xs "" = False
subString xs (y:ys) = prefix xs (y:ys) || prefix xs ys 

-- least prime divisor of n
ld :: Integer -> Integer
ld = ldf 2

-- least prime divisor greater than k of n
ldf :: Integer -> Integer -> Integer
ldf k n |
  divides k n = k
  | k^2 > n = n
  | otherwise = ldf (k+1) n

-- check if d divides n
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- check if n is prime
prime0 :: Integer -> Bool
prime0 n |
  n < 1 = error "not pos integer"
  | n == 1 = False
  | otherwise = ld n == n

-- prime factorization of a given no.
-- given n return [p1, p2 .. pn] where n = p1^a1 * p2^a2 .. * pn^an
factors :: Integer -> [Integer]
factors n |
  n < 1 = error "argument not positive"
  | n == 1 = []
  | otherwise = p : factors (div n p) where p = ld n

main :: IO ()
main = putStrLn "hi"
