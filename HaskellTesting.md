
Unit testing and property testing

spec testing- unit testing in human readable form.

Hspec, HUnit and QuickCheck.

QuickCheck relies on a typeclass called Arbitrary and a newtype
called Gen for generating its random data.

```hs
-- QuickCheck
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]
arbitrary :: Arbitrary a => Gen a
```

```hs
sample (arbitrary :: Gen Int)
{-
0
2
0
-5
-7
-6
0
9
-7
5
9
-}
```
Arbitrary provides a generator to be used for sample.