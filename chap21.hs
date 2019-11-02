module Chap21 where


    data E2 a b =
        L2 a
        | R2 b
        deriving (Eq, Ord, Show)

    instance Functor (E2 a) where
        fmap f (R2 x) = R2 (f x)
        fmap f (L2 y) = L2 y

    instance Foldable (E2 a) where
        foldr f u (L2 x) = u
        foldr f u (R2 x) = f x u
        

    main :: IO ()
    main = do
        putStrLn "Hi chap 21"