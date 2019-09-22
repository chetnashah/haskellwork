module Chap9 where

    eftChar :: Char -> Char -> String
    eftChar st end
        | st == end = []
        | (compare st end) == LT = st : eftChar (succ st) end
        | otherwise = []

    -- same as above but expressed as case expression
    eftc2 :: Char -> Char -> String
    eftc2 st end = case compare st end of
        EQ -> []
        LT -> st : eftc2 (succ st) end

    getAllWords :: Char -> String -> [String]
    getAllWords sep sentence =
        let fft = takeWhile (/=sep) sentence
            lst = dropWhile (/=sep) sentence
            in
                case sentence of
                    "" -> []
                    (hd : rest)
                        | hd == sep -> getAllWords sep rest
                        | otherwise -> fft : getAllWords sep lst

