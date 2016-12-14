
fib :: Int -> Int
fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)


  
main = do
  putStrLn "Please input a nothing"
  let arr = [(fib x)| x <- [1..35], even (fib x) && (fib x) < 4000000 ] 
  putStrLn (" ans " ++ show (sum arr))
