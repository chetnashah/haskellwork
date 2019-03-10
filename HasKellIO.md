
Print

`putStrLn: IO ()`: IO action that returns unit.

`getLine: IO String`:  IO action that returns string.

`<-` is used to pull in a value from an IO typed value.

e.g.
```hs
main = do
         putStrLn "Hello world, Who are you?"
         name <- getLine
         putStrLn "Hello" ++ name
```

