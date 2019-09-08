module Print3Broken where
    printSecond = do
        putStrLn greeting where greeting = "woah"

    main = do
        putStrLn greeting
        -- printSecond
        where greeting = "Yarr"