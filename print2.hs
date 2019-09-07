
module Print2 where
    main :: IO()
    main = do
        putStrLn "Count for me"
        let k = 99 -- let is necessary to do a declaration in a do-block
        putStrLn "one, two"
        putStrLn "three, four"
        putStrLn $ "k is " ++ show k