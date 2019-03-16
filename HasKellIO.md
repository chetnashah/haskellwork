
### Print

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

### Reading

When reading, provide a specific type you want to cast to, otherwise it
will throw error saying `no parse`.
e.g.
```hs
read "3.0" :: Float

```