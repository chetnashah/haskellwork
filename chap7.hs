module Chap7 where

    data WherePenguinsLive =
        Galapagos
        | Antarctica
        | Australia
        | SouthAfrica
        | SouthAmerica
        deriving (Eq, Show)

    data Penguin =
        Peng WherePenguinsLive
        deriving (Eq, Show)

    k (x, y) = x
    k1 = k ((4-1), 10)
    k2 = k ("three", (1 + 2))
    k3 = k (3, True)

    isPalindrome xs =
        let same = xs == reverse xs in
        case same of
            True -> "Yes"
            False -> "No"


    data Employee = Coder
                | Manager
                | Veep
                | CEO
                deriving (Eq, Ord, Show)
    
    foldBool3 :: a -> a -> Bool -> a
    foldBool3 x y z
        | z == True = x
        | z == False = y


    main = do
        putStrLn "hello"