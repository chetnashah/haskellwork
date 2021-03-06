

isPalindromeString :: [Char] -> Bool
isPalindromeString chs = (reverse chs == chs)

main = do
  let arr = [x * y | x <- [100..999], y <- [100..999], isPalindromeString(show(x * y))]
  putStrLn (show (maximum arr))
