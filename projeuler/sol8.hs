
import Data.List
import Data.Char

-- tails becomes crucial in stream like processing.
-- repeat :: a -> [a], returns an infinite list of a's
-- (zipWith fn) takes a two lists and zip them together with combiner "fn"

main = do
  contents <- getContents
  putStrLn . show
    . take 13
    . tails
    . map (fromIntegral . digitToInt)
    . concat
    . lines
    $ contents
  putStr (contents)
