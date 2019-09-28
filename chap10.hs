module Chap10 where

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f xs = foldr (\x b -> f x || b) False xs


    
    main = do
        putStrLn "hello!"