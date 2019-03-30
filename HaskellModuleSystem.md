A Haskell source file contains a definition of a single module.

A source file begins with a module declaration. This must precede all other definitions in the source file.

```hs
-- file: ch05/SimpleJSON.hs
module SimpleJSON
    (           -- enclosed the list of exports
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where ...
```

If we omit the exports (and the parentheses that enclose them) from a module declaration, every name in the module will be exported.
```hs
-- file: ch05/Exporting.hs
module ExportEverything where ...
```

To export no names at all (which is rarely useful), we write an empty export list using a pair of parentheses

```hs
-- file: ch05/Exporting.hs
module ExportNothing () where
```


### Build system

https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/

