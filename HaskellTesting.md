
Unit testing and property testing

spec testing- unit testing in human readable form.

Hspec, HUnit and QuickCheck.

QuickCheck relies on a typeclass called Arbitrary and a newtype
called Gen for generating its random data.

Use `arbitrary` to create generators from specified type.

Sample generators using `sample` or `sample'`
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

-- defaults to tuples in ghci, if type not specified
sample' arbitrary
-- [(),(),(),(),(),(),(),(),(),(),()]
-- but will throw error in ghc
```
Arbitrary provides a generator to be used for sample.

Making generators using `elements`:
```hs
elements :: [a] -> Gen a
kk = elements ['a','x','e']
sample' kk
-- "aaaxaxaeeex"
```

Using choose:
`choose`: makes a random choice of a value from a range, with a uniform distribution in the closed interval `[a, a]`
```hs
choose :: System.Random.Random a => (a, a) -> Gen a
-- we give choose a tuple containing a start value and an end value
sample' $ (choose (1, 2) :: Gen Double)
{-
[1.4355000447858492,1.4491930531493835,1.219811220463531,1.0347449239560924,1.7223233486928542,1.9933094279815102,1.0921965842747352,1.1387482313911679,1.3333408031857679,1.378731987652147,1.5081451590862542]
-}
```

Generators for data structures:
e.g. `Maybe`
```hs
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

sample' (genMaybe :: Gen (Maybe Integer))
{-
[Nothing,Just 2,Nothing,Just (-5),Nothing,Just (-9),Just 4,Nothing,Nothing,Just (-3),Just 16]
-}
```

`frequency`: Chooses one of the given generators, with a weighted random distribution.

```hs
frequency :: [(Int, Gen a)] -> Gen a

abHeavyGen = frequency [(1,elements ['x','y']), (5, elements ['a','b'])]

sample' abHeavyGen
-- "bxaxaabbxbb"
```

### Using quickcheck with Hspec

```hs
-- library function quickCheck takes a testable property
-- quickCheck :: Testable prop => prop -> IO ()

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
```

Too see all tests with description,
use `verboseCheck prop_`.

Testing a monoid law:
```hs
import Data.Monoid
import Test.QuickCheck
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

quickCheck (monoidLeftIdentity :: String -> Bool)
quickCheck (monoidRightIdentity :: String -> Bool)

```

### Implementing instances for Arbitrary typeclass

Typeclass source:
```hs
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]

-- some instance implementations
instance Arbitrary Bool where
  arbitrary = choose (False,True)

instance Arbitrary Ordering where
  arbitrary = elements [LT, EQ, GT]

-- when testing for custom data types, 
-- good idea to implement Arbitrary.
data Bull = Fools | Twoo deriving (Eq, Show)
instance Arbitrary Bull where
arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]
```



