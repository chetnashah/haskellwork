
module Exercises3 where

    thirdLetter :: String -> Char
    thirdLetter x = x !! 3

    letterIndex :: Int -> Char
    letterIndex x = "Curry is Awesome!" !! x

    rvrs :: String -> String
    rvrs xs = let first = take 5 xs
                  last = drop 9 xs
                  middle = take 4 $ drop 5 xs
              in last ++ middle ++ first 

    main = do
        putStrLn $ show $ thirdLetter "Curry is awesome"
        putStrLn $ show $ letterIndex 0
        putStrLn $ rvrs "Curry is awesome"