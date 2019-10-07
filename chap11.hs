{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap11 where

    data PugType = PugData

    -- type parameter a is a phantom here
    data HuskyType a = HuskyData

    data DogueDeBordeaux doge = DogueDeBordeaux doge deriving Show

    myDoge :: DogueDeBordeaux Int
    myDoge = DogueDeBordeaux 10

    badDoge :: DogueDeBordeaux String
    badDoge = DogueDeBordeaux "jkh"

    data Doggies a =
        Husky a
        | Mastiff a
        deriving (Eq, Show)

    data Manufacturer =
        Mini
        | Mazda
        | Tata
        deriving (Eq, Show)

    data Price =
        Price Integer deriving (Eq, Show)

    data Airline =
        PapuAir
        | CatapultsRUs
        | TakeYourChancesUnited
        deriving (Eq, Show)
    
    data Vehicle = Car Manufacturer Price
        | Plane Airline Integer
        deriving (Eq, Show)


    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge :: Vehicle
    doge = Plane PapuAir 90

    isCar :: Vehicle -> Bool
    isCar v = case v of
        Car _ _ -> True
        _ -> False

    isPlane :: Vehicle -> Bool
    isPlane v = case v of
        Plane _ _ -> True
        _ -> False 

    areCars :: [Vehicle] -> [Bool]
    areCars vs = map isCar vs

    getManu :: Vehicle -> Manufacturer
    getManu v = case v of
        Car m _ -> m
        _ -> undefined


    newtype Cows = Cows Int deriving (Eq, Show)

    tooManyGoats :: Goats -> Bool
    tooManyGoats (Goats n) = n > 42

    class TooMany a where
        tooMany :: a -> Bool
    
    -- tooMany :: TooMany a => a -> Bool

    instance TooMany Int where
        tooMany n = n > 42
    -- tooMany (33 :: Int)

    -- instance TooMany Goats where
    --     tooMany (Goats n) = n > 43

    -- tooMany (Goats 22)
    newtype Goats = Goats Int deriving (Eq, Show, TooMany) -- only works if GeneralizedNewtypeDeriving is on

    instance TooMany (Int, String) where
        tooMany (n,s) = n > 11
    
    instance (Num a, TooMany a) => TooMany (a, a) where
        tooMany (b,c) = True
    
    data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

    data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

    -- automatically introduces functions name and age
    data Person = MkPerson { name :: String, age :: Int } deriving (Eq, Show)

    -- create data value same as earlier, matching by position
    p1 = MkPerson "John" 22
    p1Name = name p1
    p1Age = age p1

    data Fiction = Fiction deriving Show
    data NonFiction = NonFiction deriving Show
    data BookType = FictionBook Fiction
        | NonFictionBook NonFiction
        deriving Show
    
    type AuthorName = String
    data Author = Author (AuthorName, BookType) -- product type name * book

    data Author2 = -- sum type fiction with name or non-fiction with name
        Fiction2 AuthorName
        | NonFiction2 AuthorName
    
    data Expr =
        Number Int
        | Add Expr Expr
        | Minus Expr
        | Mult Expr Expr
        | Divide Expr Expr

    data FlowerType = Gardenia
        | Daisy
        | Rose
        | Lilac
        deriving Show
    
    type Gardener = String

    data Garden =
        Garden Gardener FlowerType
        deriving Show
    
    data BinaryTree a = Leaf
        | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)


    insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
    insert' x Leaf = Node Leaf x Leaf
    insert' x tree = case tree of
        (Node l n r)
            | x > n -> Node l n (insert' x r)
            | x < n -> Node (insert' x l) n r
            | otherwise -> Node l n (insert' x r)

    map' :: (Ord a) => (a -> b) -> BinaryTree a -> BinaryTree b
    map' f Leaf = Leaf
    map' f (Node l a r) = Node (map' f l) (f a) (map' f r)

    preorder :: BinaryTree a -> [a]
    preorder Leaf = []
    preorder (Node l a r) = [a] ++ preorder l ++ preorder r 

    inorder :: BinaryTree a -> [a]
    inorder Leaf = []
    inorder (Node l a r) = inorder l ++ [a] ++ inorder r

    testTree :: BinaryTree Integer
    testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

    -- TODO
    -- use any traversal to fold
    -- foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf [] [] = True
    isSubsequenceOf (x:_) [] = False
    isSubsequenceOf [] (y:_) = True
    isSubsequenceOf (x:xs) ys = (x `elem` ys) && isSubsequenceOf xs ys

    type Digit = Char
    type Presses = Int

    type PhoneValues = String

    data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

    daPhone = DaPhone
        [('1', "1"),
        ('2',"abc"),
        ('3', "def"),
        ('4',"ghi"),
        ('5', "jkl"),
        ('6', "mno"),
        ('7', "pqrs"),
        ('8', "tuv"),
        ('9', "wxyz"),
        ('*', "*^"),
        ('0', "+_"),
        ('#', "#.,")
        ]

    convo :: [String]
    convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

    -- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]


    -- findPair :: DaPhone -> Char -> 


    -- Hutton's Razor
    data Expression =
        LitExp Integer
        | AddExp Expression Expression
    
    eval :: Expression -> Integer
    eval (LitExp i) = i
    eval (AddExp e1 e2) = (+) (eval e1) (eval e2)

    printExpr :: Expression -> String
    printExpr (LitExp i) = show i
    printExpr (AddExp e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

    main = do
        putStrLn "Hello world!"