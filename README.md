
### REPL

ghci is repl for haskell

* use `:t` variable to get type
* use `:k` variable to get kind
* use `:i` binding to get info on binding
* use `:l` filename to load `.hs` file into repl

faster docs is accessed by SPC m h H

Write code blocks in `ghci` using `:{` and `:}`.
e.g.
```sh
prelude>:{
  do
    putStrLn "hello world"
:}
```

* The `it` value in ghci: `it` is the last evaluated expression in ghci.


### Prefix and infix functions

All functions are prefix functions by default and calling syntax is
`fn arg1 arg2`. To use a prefix function as an infix form, you  put it in back quotes e.g . `fn arg1 arg2` is same as ``arg1 `fn` arg2``

To use an infix function, you put it at first position with paranthenses
e.g. `(+) 2 6` is same as `2 + 6`


### Haskell Stack
Recommended tool for beginners to manage tools/dependencies/projects.

### Top level expressions

 The interactive environment ghci would lead you to believe that you can punch some expressions into an .hs file and run the thing (in a similar fashion to languages like swift and ruby). This is not the case.

Haskell needs an entrypoint called `main`.
Instead `module header, import declaration or top-level declaration expected`.

In haskell you can put a top-level binding/declaration e.g. `k = 99`, but you cannot put a top-level expression like `putStrLn "wow`.



### Basics

* blocks are indentation based.
* Space ` ` stands for function application.
* To force execution order, use `()` round parentheses, default execution order is lazy (undetermined depending on runtime)

* `$` vs. `()`: The `$` is for avoiding parantheses. Anything that comes after `$` will be evaluated first.
e.g. all below are quivalent
```hs
putStrLn (show (1 + 1))
putStrLn $ show (1 + 1)
putStrLn $ show $ 1 + 1
```

* function is defined with `=`. i.e.
A function definition:
1. starts with the name of the function,
2. followed by its formal parameters separated by white spaces,
3. an equal sign,
4. and an expression that is the body of the function.

e.g. 
`a b c = d e`

* Function application -- in most cases just the "whitespace operator" --has the highest precedence

* There is no order of execution (lazy)

* Order of evaluation is present in `do`.

### Reading error messages

Understanding bottom-up typechecking and unification can help in polymorphic cases.s

http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_understanding_basic_haskell_error_messages.pdf

https://stackoverflow.com/questions/10375532/haskell-understanding-no-instance-for-error-messages-in-ghci

The `(b ~ a)` notation indicates equality
of the types a and b.

The term `rigid type variable` indicates
that our types a and b have been directly specified by the type annotation [5] and
the compiler is not free to unify them.


Telling compiler that unification can happen:
```hs
{-# LANGUAGE TypeFamilies #-}
isEq :: (Eq a, Eq b, a ~ b) => a -> b -> Bool
isEq x y = x == y
```

### Runtime exceptions

1. non total functions - usually warned during compilation by non-exhaustive
pattern matching.

2. 

### Reading types

The constraints are specified before `=>` and the type signature
is shown after `=>`.

One can see the type of any binding using `:t` on terminal.

```hs
show :: Show a => a -> String
-- show takes a binding (which should be a Showable) and returns a string represetnation of that binding.
```

#### Verifying type

```hs
-- :t 1 returns Num
-- but 1 :: Num throws error.

1 :: Int -- does not throw any error
1 :: Bool -- throws error
```

#### common types

1. concat operator for lists

```hs
-- :t (++)
(++) :: [a] -> [a] -> [a]
```

2. `String` is a type alias for `[Char]`
```hs
-- :t 'A'   -- Char
-- :t "A"   -- [Char]
```

3. readables are read using `read` which takes in a string and returns
```hs
-- :t read
-- read :: Read a => String -> a
```

4. fromInteger
```hs
instance Num x where
    fromInteger :: Integer -> x
```
Whenever `x` doesn not confirm to Num,
we get erros like `No instance for (Num [Char])` etc. when doing `3 + "4"`

5. cons symbol `(:)`
```hs
-- :t (:)
-- (:) :: a -> [a] -> [a]
```

6: putStrLn
```hs
-- putStrLn :: St
String -> IO ()
```

7. getLine
```hs
-- getLine :: IO String
-- value from an IO is extracted using <-
```

### Bindings

Using `let` and `where` and `pattern matching` by defining functions,
We introduced `bindings`. Bindings are essentially names for expressions.
And cannot be rebound in same scope.

#### let expression

`let` expression kinds of act as a lambda, that introduces `scope`.

`let ... in ...` is an expression, that is, it can be written wherever expressions are allowed.

Scope of bindings declared in let, are only valid till the body of let expression.
e.g
```hs
j = 4
main = do
  let k = 2
  let u = 4 in
    putStrLn $ show $ k + j + u
  putStrLn $ show k
  putStrLn $ show j
  -- putStrLn $ show u -- u not in scope
```

Multiline `let` can be achieved by using careful indentation:
```hs
up arg1 =
  let p = 22
      t = 11
      in putStrLn $ show $ p + t

-- up 0
-- 33
```

**let without `in`**: introduces binding in the current scope.


#### where bindings vs let bindings