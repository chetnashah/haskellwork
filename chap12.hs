module Chap12 where


    type Name = String
    type Age = Integer

    data Person = Person Name Age deriving Show
    data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq)

    toString :: PersonInvalid -> String
    toString NameEmpty = "NameEmpty"
    toString AgeTooLow = "AgeTooLow"

    instance Show PersonInvalid where
        show = toString

    mkPerson :: Name -> Age -> Either PersonInvalid Person
    mkPerson name age
        | name /= "" && age >= 0 = Right $ Person name age
        | name == "" = Left NameEmpty
        | otherwise = Left AgeTooLow
    
    isVowel :: Char -> Bool
    isVowel x = x `elem` "aeiou"

    -- returns Nothing if given string is starts with "the " follwoed by a vowel
    -- otherwise returns the phrase itself
    befoeWovel :: String -> Maybe String
    befoeWovel phrase@('t':'h':'e':' ':x:xs)
        | isVowel x = Nothing
        | otherwise = Just phrase
    befoeWovel xs = Just xs

    countBeforeWovel :: String -> Integer
    countBeforeWovel "" = 0
    countBeforeWovel xs = case befoeWovel xs of
        Nothing -> 1 + countBeforeWovel (drop 4 xs)
        Just (y:ys) -> 0 + countBeforeWovel ys

    data Nat = Zero | Succ Nat deriving (Eq, Show)

    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ x) = 1 + natToInteger x

    integerToNat :: Integer -> Maybe Nat
    integerToNat 0 = Just Zero
    integerToNat n
        | n < 0 = Nothing
        | otherwise = Just (iNat n)
        where
            iNat k
                | k == 0 = Zero
                | k > 0 = Succ (iNat (k-1))

    maybee :: b -> (a -> b) -> Maybe a -> b
    maybee x f v = case v of
        Nothing -> x
        (Just y) -> f y

    fromMaybe :: a -> Maybe a -> a
    fromMaybe x v = maybee x id v

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:xs) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

    catMaybes ls = [x | Just x <- ls]

    mysequence :: (Eq a) => [Maybe a] -> Maybe [a]
    mysequence allxs@(x:xs)
        | null allxs = Just []
        | Nothing `elem` allxs = Nothing
        | otherwise = Just (simpleseq [] allxs)
            where
                simpleseq ans zs = case zs of
                    [] -> ans
                    (Just y:ys) -> simpleseq (ans ++ [y]) ys


    -- lefts' [Left 3, Right "Hi", Left 7] = [3,7]
    lefts' :: [Either a b] -> [a]
    lefts' xs = foldr cmb [] xs
        where
            cmb a b = case a of
                Left x -> x:b
                (Right y) -> b

    rights' :: [Either a b] -> [b]
    rights' = foldr cmb []
        where
            cmb a b = case a of
                Left x -> b
                Right y -> y:b
    
    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' xs = (lefts' xs, rights' xs)

    -- If Either is right, give a just value after applying function
    -- and if EIther is wrong/left, give nothing
    -- eitherMaybe' (+1) Right 2 = Just 3
    -- eitherMaybe' (+1) Left "Ignored" = Nothing
    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' f e = case e of
        Left x -> Nothing
        Right y -> Just (f y)

    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' f1 f2 x = case x of
        Left y -> f1 y
        Right z -> f2 z

    eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe'' f e = either' (const Nothing) (Just . f) e

    myIterate :: (a -> a) -> a -> [a]
    myIterate f s = s : myIterate f (f s)

    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f s = case f s of
        Nothing -> []
        (Just (x, y)) -> x : myUnfoldr f y


    -- betterIterate using myUnfoldr
    betterIterate :: (a -> a) -> a -> [a]
    betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x


    data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)

    unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
    unfoldTree = undefined

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild = undefined

    

    main = do
        putStrLn "hello world"