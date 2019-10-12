
Unit testing and property testing

spec testing- unit testing in human readable form.

Hspec, HUnit and QuickCheck.

QuickCheck relies on a typeclass called Arbitrary and a newtype
called Gen for generating its random data.

```hs
-- QuickCheck
sample :: Show a => Gen a -> IO () -- generates and prints 10 random values of type a
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

sample' (arbitrary :: Gen Double)
{-
[0.0,-0.2844295008675959,0.10667183469386676,5.889311980575827,-5.8268598472218525,-4.7254313280530456,9.152926452122317,-6.2092150878914065,-6.641874394650961,-5.055836213905848,-13.767724103274583]
-}
```
Arbitrary provides a generator to be used for sample.