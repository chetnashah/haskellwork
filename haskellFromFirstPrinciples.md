
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


