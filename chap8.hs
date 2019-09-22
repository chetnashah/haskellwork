

module Chap8 where

    import Data.List (intersperse)

    dividedBy :: Integral a => a -> a -> (a, a)
    dividedBy num denom = 
        let n1 = num
            d1 = denom
            ans1 = (0, 0)
            divBy ::  Integral a => a -> a -> (a, a) -> (a, a)
            divBy n d ans
                | (n - d) == 0 = (fst ans + 1, snd ans)
                | (n - d) < 0 = (fst ans, n)
                | (n - d) > 0 = divBy (n - d) d (1 + fst ans, snd ans)
            in divBy n1 d1 ans1

    digitToWord :: Int -> String
    digitToWord n = undefined

    digits :: Int -> [Int]
    digits n = undefined

    wordNumber :: Int -> String
    wordNumber n = undefined

    main :: IO ()
    main = do
        putStrLn "hi world"