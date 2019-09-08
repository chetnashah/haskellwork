module Reverse where

    rvrs :: String -> String
    rvrs xs = let first = take 5 xs
                  last = drop 9 xs
                  middle = take 4 $ drop 5 xs
              in last ++ middle ++ first 

    main :: IO ()
    main = print ()