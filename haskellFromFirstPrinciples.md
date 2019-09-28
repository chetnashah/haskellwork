
### Type constraints

```hs
(Ord a, Num a) => a -> a -> Ordering
```

The tuple of constraints does
represent a product, or conjunction, of constraints.

In case of type signatures with multiple arguments,
type bracketing happens at the right.
e.g.
```hs
f :: a -> a -> a
-- associates to
f :: a -> (a -> a)

map :: (a -> b) -> [a] -> [b]
-- associates into
map :: (a -> b) -> ([a] -> [b])
```
This kind of bracketing helps us understand that each `->` only has one argument 
and one result i.e. currying is happening.


### Currying and Uncurrying

```hs
let curry f a b = f (a, b)
curry :: ((t1, t2) -> t) -> t1 -> t2 -> t

let uncurry f (a, b) = f a b
uncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t
```


### Monomorphism restriction

If you forget to provide a type signature, sometimes this rule will fill the free type variables with specific types using "type defaulting" rules. The resulting type signature is always less polymorphic than you'd expect, so often this results in the compiler throwing type errors at you in situations where you expected it to infer a perfectly sane type for a polymorphic expression.

The restriction is turned on by default in compiled modules, and turned off by default at the GHCi prompt.

In GHCi:
```hs
let pp = (+)
-- :t pp = Num a => a -> a -> a
```

In compile modules:
```hs
let pp = (+)
-- :t pp = Integer -> Integer -> Integer
```

### Type polymorphism

ith respect to Haskell, the principal type is the most generic
type which still typechecks. More generally, Principal type is a
property of the type system you’re interacting with. Principal
typing holds for that type system if a type can be found for a
term in an environment for which all other types for that term
are instances of the principal type.

### Typeclasses

Typeclass instances we can magically derive
are `Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`

Unlike other languages, Haskell does not provide universal stringification (Show / print) or equality (Eq (value equality) or pointer equality) as this is not always sound or safe

### Typeclass defaulting and co-ercion

Below are the given default values 
for given typeclass:
```hs
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

### Ord typeclass

Ord extends upon `Eq`, since it is necessary
to have equality comparision in order to order elements.

Interface method: `compare`
`compare` returns one of `EQ`, `LT`, `GT`.

Other operators include comparision operators:
`>`, `<` which returns a `Bool`.

`max` and `min` operators return value of same polymorphic `a`.

Compound types like `[]`, `(,)`, `Either` etc can be Ord by peicewise Ord elements compared respectively.

### Explicit type annotation in Ghci

```hs
> let x::Int; x = read "1"
> x
1
```

### Data type definition

To the left side of the datatype definition, we are allowed the type constructor,
and type variables.

To the right side of the equals sign of datatype definition, we are allowed
data constructors along with `typevariables/concreteType/ExistingDataType`.

### 

Type checker will try to give variables the most generic/general/maximally polymorphic type based on the inferred knowledge.


We can declare more specific (monomorphic) functions from
more general (polymorphic) functions:
```hs
-- polymorphic id function
myid :: a -> a
myid x = x
-- co-erced to a specific type
(myid :: Integer -> Integer) 33 -- 33
```

Not the other way round:
we lost the generality
of `Num` when we specialized to `Integer`

```hs
let numId = id :: Num a => a -> a
let intId = numId :: Integer -> Integer
-- below line throws error
let altNumId = intId :: Num a => a -> a
-- throws long type error
```

Note: **Concrete types imply all the typeclass they provide**

e.g. 
```hs
-- since Int has Ord and Num instance, everything works
addWeird :: Int -> Int -> Int
addWeird x y =
if x > 1
then x + y
else x
```

### Pattern matching

Patterns are matched against values or data constructors, not types.

Pattern matching proceeds from left to right and outside to inside.

Data constructors come in handy on the left side of equal sign,
acting to destructure function arguments which are values made using 
data constructors.

Basically any destructuring to left of equal e.g. `(,)` does tuple destructuring
`[]` does array destructuring . any user defined data constructor to left of equal sign 
destructure passed in argument

### Case expressions

Case expressions go well with pattern matching:
The value of case expression is the value of the expression corresponding to 
the pattern that was successfully matched.
```
case expr of
    pt1 -> expr1
    pt2 -> expr2
```
**Note** - No pipes in case expressions like they are present in guards
Case expression along with let :
```hs
    isPalindrome xs =
        let same = xs == reverse xs in
        case same of
            True -> "Yes"
            False -> "No"
```

One can use `where` clause with case expressions
```hs
    isPalindrome xs =
        case same of
            True -> "Yes"
            False -> "No"
            where same = xs == reverse xs
```

One cannot match against variable values

e.g.
**will not work**
```hs
let kk = 1
let abc = 3
case abc of
    kk -> "is one"
    _ -> "not one"
```

Case expressions can have guards but in an interesting way:
whcih means case expr patters allow boolean guards!! which is quite handy
(https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)
e.g.
```hs
    -- sep means separator
    getAllWords :: Char -> String -> [String]
    getAllWords sep sentence =
        let fft = takeWhile (/=sep) sentence
            lst = dropWhile (/=sep) sentence
            in
                case sentence of
                    "" -> []
                    (hd : rest)
                        | hd == sep -> getAllWords sep rest
                        | otherwise -> fft : getAllWords sep lst
```

### Guards

Guards are basically constraints for patterns.
Allowed along side function definitions with `=`.
and allowed along side case patterns with `->`.

`otherwise` is an alias for `True`
e.g.
```hs
isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Right ON"
    | otherwise        = "Not right"
```
**Note** - Equals sign appears in the statement sets, not before pipes

```hs
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100
```

Guards are not very good at exhaustive pattern matching compared `case` expressions,
so you must add `otherwise` at the last guard to fix warning `-Wincomplete-patterns`.

Multi argument guard expr:
```hs
    fB3 :: a -> a -> Bool -> a
    fB3 x y z
        | z == True = x
        | z == False = y
```

`Case with guards`:

Here we see pattern matches but guard does not, so
interpreter moves on to next pattern which is `_`.
```hs
kk = 2
case kk of
  1 -> "I am one"
  2   
    | False -> "Two but false"
  _ -> "I am rest of case"
-- prints "I am rest of case"
```



#### pattern guards

pattern guards are of the form `p <- e`, where `p` is a pattern (see Section 3.17) of type `t` and `e` is an expression type `t1`.
We usually see them in list comprehensions

#### Boolean guards

boolean guards are arbitrary expressions of type `Bool`.


### Higher order functions

To better understand how HOFs work syntactically, it’s worth
remembering how parentheses associate in type signatures.

```hs
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

When there are no higher order functions, by default the brackets
associate to the right in type signatures, since all multi argument
signatures are curried implicitly


### Function composition

Remember in mathematics we use to have `g.f x`, which is also read as `g after f` on x.

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
```

All functions participating in composition should be single argument and single return

Usage of `$` along with composition
```hs
negate . sum $ [1,2,3,4,5]
(negate . sum ) [1,2,3,4,5]
negate (sum [1,2,3,4,5])
-- what happens without $
-- negate . (sum [1,2,3,4,5])
-- negate . 15
-- error since 15 is not a function
```

Interesting example:
```hs
 take 5 . filter odd . enumFrom $ 3
```

`print = (putStr . show)`
`print :: Show a => a -> IO ()`
`show :: Show a => a -> String`
`putStr :: String -> IO ()`
The point of print is to compose putStrLn and show so that we don’t
have to call show on its argument ourselves

### Recursion and Y Combinator


### Bottom

`_|_` or `bottom` is a term used in haskell to refer to computations that do
not successfully result in a value.

Throw exceptions using `error strMsg`
e.g.
```hs
f :: Bool -> Int
f True = error "blah"
f False = 0
```

### Lists

Definition:
```hs
data [] a = [] | a : [a]
```

Haskell has some syntactic sugar to accommodate the use of lists
```hs
(1 : 2 : 3 : []) ++ (4 : [])
-- is same as
[1, 2, 3] ++ [4]
```

Range syntaxes:
```hs
[1..10] -- [1,2,3,4,5,6,7,8,9]
enumFrom 1 -- [1,2,3............Infinity]
enumFromTo 1 10 -- [1,2,3,4,5,6,7,8,9]
enumFromThenTo 1 3 10 -- [1,3,5,7,9]
```

Extracting portions of list
```hs
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
```

Taking/dropping/splitting out of empty list returns an empty list

Instead of specifying exact numbers to take drop, a useful version is
Predicate based taking/dropping
```hs
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
```
while iteration stops as soon as any iteratio returns false.
e.g.
```hs
takeWhile (=='a') "abracadabra"
-- a
takeWhile (>6) [1..10]
-- []
```

### List comprehensions

Simple list comprehension

```hs
[x^2 | x <- [1..10]]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

List comprehension with filter/predicates

```hs
[x^2 | x <- [1..10], rem x 2 == 0] -- filter out even ones from generated nums, then feed to function
-- [4, 16, 36, 64, 100]
```

Multiple generators, think of loop nesting in same manner as order found in the list comprehension:
```hs
[x^y | x <- [1..5], y <- [2, 3]]
-- [1, 1, 4, 8, 9, 27, 16, 64, 25, 125] 
```

Multiple generators with a filter
```hs
[x^y | x <- [1..10], y <- [2, 3], x^y < 200]
-- [1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]
```

List comprhension with strings:
```hs
[x | x <- "Three Letter Acronym", elem x ['A'..'Z']]
-- "TLA"
```

#### List evaluation

The cons `:` can be evaluated lazily without evaluating elements.
e.g. `length [1, undefined, 3]` will not crash
Seeing properly
```hs
length [] = 0
length (_:xs) = 1 + length xs -- ignoring heads, so undefined is fine, only care about pattern matching cons
```

#### Transforming list values

`map` is only for arrays
`fmap` is for all kinds of functors , array is a functor.

#### zipping lists

zip takes a bunch of lists and zips them together into a single item
like a chain/zip does. the order of items in tuple is same as input order
```hs
zip :: [a] -> [b] -> [(a, b)]
```
**Note** - zip stops as soon as one of the lists runs out of values
e.g.
```hs
zip [1,2,3] [99]
-- [(1,99)]
```

`zipWith`: Takes a binary function,
and two lists, and returns
the list which is generated by applied function.

#### Folding

Think of it like this:
Instead of reconsing when mapping, a combiner is applied to reduce a list to a value.
Folding expects identity satisfied by combiner as a starting element.
E.g. identity for `(+)` is `0`.
Identity for `(*)` is `1`.
Identity for `++` is `[]`.

Implementation:
```hs
-- note in both cases (foldl and foldr), we deconstruct cons from left to right.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
-- expansion in foldr looks like
-- f x1 (f x2 (f x3 (f x4 (f x5 ... (f xn acc)))))

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
foldl :: (b -> a -> b) -> b -> [a] -> b

-- expansion in foldl looks like
-- f (f (f (f (f ... (f acc x1) x2) x3) x4).. xn)
```

It is right associative, i.e. right most reductions happen first.
e.g.
`foldr (+) 0 [1, 2, 3]`
Into,
`(+) 1 ((+) 2 ((+) 3 0))`

Given this two-stage process and non-strict evaluation, if 𝑓 doesn’t
evaluate its second argument (rest of the fold), no more of the spine
will be forced. One of the consequences of this is that foldr can avoid
evaluating not just some or all of the values in the list, but some or
all of the list’s spine as well

Folding over infinite lists:
It is possible to fold over infinite lists, where combiner are lazy and/or
do not care about rest of the sequence, e.g. `||` or `&&`

Predicting type of `foldr`, `foldl`:

1. combiner function: for foldr, it is `lv -> iv -> iv` (unit comes after list, right type is preferred), 
for foldl, it is `iv -> lv -> iv` (unit comes before lists, left type is return)
2. initial value: type `a`.
3. list values: type `[b]`.
4. return value : same type as return value always. type a.

```hs
-- derived from above
foldl:: (a -> b -> a) -> a -> [b] -> a
     -- replacing consistently
     :: (b -> a -> b) -> b -> [a] -> b


foldr:: (b -> a -> a) -> a -> [b] -> a
    -- replacing consistently
    :: (a -> b -> b) -> b -> [a] -> b
```


`foldr (\_ _ -> 9001) 0 [undefined]` or `foldr (\_ _ -> 9001) undefined [undefined]` returns 9001 due to lazy evaluation.
since folding function does not care about its arguments
but `foldr (\_ _ -> 9001) 0 undefined` will throw error.
