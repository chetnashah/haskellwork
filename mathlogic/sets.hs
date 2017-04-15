-- idea is using lists to represent sets
-- sets don't have orders and sets don't have duplicates
-- delete will remove first instance of an element found in list by equality
-- all definitions in this file behave same as that in std lib Data.List
main = do
  putStrLn "hi"


-- deleteL operation deletes the first occurence of given item in list
-- by equality comparision, and returns same list if not found
-- makes a single iteration through the list
deleteL :: Eq a => a -> [a] -> [a]
deleteL y [] = []
deleteL y (x:xs) | x == y = xs
                | otherwise = x : deleteL y xs

-- elemL is a predicate that takes a given element and a list and
-- uses equality comparision to determine if an element is present in the list
-- makes a single iteration through the list
elemL :: Eq a => a -> [a] -> Bool
elemL y [] = False
elemL y (x:xs) | x == y = True
               | otherwise = elemL y xs


-- union operation in std lib Data.List does union of two given lists
-- also removing duplicates from second list in the process
-- but if the first list contains duplicates, so will the result
-- e.g. union "doooooge" "ceeeeow"
-- "dooooogecw"


-- makes a single iteration each of which makes nested iteration via delete
unionL :: Eq a => [a] -> [a] -> [a]
unionL [] ys = ys
unionL (x:xs) ys = x : unionL xs (deleteL x ys)

-- intersect takes two lists and gives their intersection
-- removing duplicates of second
-- if first list contains duplicates so will the result

intersectL :: Eq a => [a] -> [a] -> [a]
intersectL [] ys = []
intersectL (x:xs) ys | elemL x ys = x : intersectL xs ys
                     | otherwise = intersectL xs ys

-- not a stdLib function
-- addElem adds an element to front of each list in a list of lists
addElem :: a -> [[a]] -> [[a]]
addElem x = map (x:)

-- calculation powersets of given set
powerSet :: [a] -> [[a]] -- note the type follows from cantor's theorem
powerSet [] = [[]] -- powerset of empty set is set containing empty set
powerSet (x:xs) = powerSet xs ++ (map (x:) (powerSet xs))
-- structurally smaller set along with element consed to every structurally
-- smaller set

