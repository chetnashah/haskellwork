
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