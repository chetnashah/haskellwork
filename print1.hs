module Print1 where

k = (/) 6 2

h :: (Num a, Num b) => a -> b -> b
h _ y = y

jackal :: (Ord a, Eq b) => a -> b -> a
jackal x y = x

kessel :: (Ord a, Num b) => a -> b -> a
kessel x y = x

-- :t kessel 1 2
-- (Ord a, Num a) => a

-- :t kessel 1 (2 :: Integer)
-- (Ord a, Num a) => a

-- :t kessel (1 :: Integer) 2 
-- Integer

main :: IO ()
main = putStrLn "hello world!"