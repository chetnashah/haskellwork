
sq x = x * x

main = do
  putStrLn $ show  $ sq (sum [1..100]) - sum (map sq [1..100]) 
