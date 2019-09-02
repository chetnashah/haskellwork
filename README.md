
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


### Kinds

To ensure that they are valid, type expressions are classified into different kinds, which take one of two possible forms:

The symbol `∗` represents the kind of all nullary type constructors.

Char, Int, Integer, Float, Double and Bool are type constants with kind `∗`.

If `κ1` and `κ2` are kinds, then `κ1 → κ2` is the `kind of types` that `take a type of kind κ1` and `return a type of kind κ2`.

Maybe and IO are unary type constructors, and treated as types with kind `∗ → ∗`

1. The trivial type is written as `()` and has `kind ∗`. It denotes the `nullary tuple` type, and has exactly one value, also written (). (think already a concrete type)
2. The function type is written as `(->)` and has kind `∗ → ∗ → ∗`. (think that it takes two concrete types and returns a new concrete function type)
3. The list type is written as `[]` and has kind `∗ → ∗`. (think it takes a concrete type and returns the concrete array type)
4. The tuple types are written as `(,)`, `(,,)`, and so on. Their kinds are `∗→∗→∗`, `∗→∗→∗→∗`, and so on.
5. `Type application`: Type application. If `t1` is a type of kind `κ1 → κ2` and `t2` is a type of kind `κ1`, then `t1 t2` is a type expression of kind `κ2`.
6. A parenthesized type, having form `(t)`, is identical to the type `t`.



Kind inference checks the validity of type expressions in a similar way that type inference checks the validity of value expressions. However, unlike types, kinds are entirely implicit and are not a visible part of the language

### Operators

An operator is a function that can be applied using infix syntax (Section 3.4), or partially applied using a section (Section 3.5).

An operator is either an operator symbol, such as + or $$, or is an ordinary identifier enclosed in grave accents (backquotes), such as ` op `. For example, instead of writing the prefix application op x y, one can write the infix application x` op ` y. If no fixity declaration is given for ` op ` then it defaults to highest precedence and left associativity 

Dually, an operator symbol can be converted to an ordinary identifier by enclosing it in parentheses. For example, `(+) x y` is equivalent to `x + y`, and `foldr (⋆) 1 xs` is equivalent to `foldr (\x y -> x⋆y) 1 xs`.



### Values and types

An expression evaluates to a value and has a static type. 
Values and types are not mixed in Haskell. 

However, the type system allows user-defined datatypes of various sorts, and permits not only parametric polymorphism (using a traditional Hindley-Milner type structure) but also ad hoc polymorphism, or overloading (using type classes).

Errors in Haskell are semantically equivalent to ⊥ (“bottom”). Technically, they are indistinguishable from nontermination, so the language includes no mechanism for detecting or acting upon errors.

### Namespaces

There are six kinds of names in Haskell: those for `variables` and `constructors` denote values; those for `type variables`, `type constructors`, and `type classes` refer to entities related to the type system; and `module` names refer to modules. There are two constraints on naming:

1. `Names for variables and type variables are identifiers` beginning with `lowercase letters or underscore`; 
the `other four kinds of names` are identifiers beginning with `uppercase letters`.

2. An `identifier must not be used as the name of a type constructor and a class` in the same scope.
These are the only constraints; for example, Int may simultaneously be the name of a module, class, and constructor within a single scope.

### Function application and definition

Function application is written `e1 e2`. 
Application associates to the left, so the parentheses may be omitted in `(f x) y`. Because e1 could be a data constructor, partial applications of data constructors are allowed.

Lambda abstractions are written `\ p1 … pn -> e`, where the `pi` are patterns. An expression such as `\x:xs->x` is syntactically incorrect; it may legally be written as `\(x:xs)->x`.


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

8. `sequence` in `Control.Monad`
Converting traversable monads to monadic traversable.
```hs
-- sequence :: (Traversable t, Monad m) =>
-- t (m a) -> m (t a)
-- e.g. [IO Char] -> IO [Char]
```


### Bindings

Using `let` and `where` and `pattern matching` by defining functions,
We introduced `bindings`. Bindings are essentially names for expressions.
And cannot be rebound in same scope.

### Reverse thin arrow operator `<-`

In case of List comprehensions and case guards,
`<-` means a pattern match.

In case of mondadic values, `<-` means run an action.

#### let expression

`let` expression kinds of act as a lambda, that introduces `scope`.

`let ... in ...` is an expression, that is, it can be written wherever expressions are allowed.

Let unlike in other languages, is lazy in haskell,

Below code does not cause error until x or y is evaluated.
```hs
let (x,y) = undefined in e
```

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

### when to use a `do` block?

When doing a bunch of computation, and then returning a value via `return`. We need to combine all of it into a single expression via `do` block.
Especially useful in `let declarations in expression`.
or `resultExpression` in
```hs
case expression of pattern -> resultExpression
                   pattern -> resultExpression
                   pattern -> resultExpression
```

### List comprehensions

A list comprehension has the form `[ e | q1, ..., qn ], n>=1`, where the qi qualifiers are either

1. generators of the form `p <- e`, where `p` is a pattern (see Section 3.17) of `type t` and `e` is an expression of `type [t]`
2. guards, which are arbitrary expressions of type Bool
3. local bindings that provide new definitions for use in the generated expression e or subsequent guards and generators.

Such a list comprehension returns the list of elements produced by evaluating e in the successive environments created by the nested, depth-first evaluation of the generators in the qualifier list. Binding of variables occurs according to the normal pattern matching rules (see Section 3.17), and if a match fails then that element of the list is simply skipped over. Thus: 
```hs
[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], 
      (3,x) <- xs ]
```
gives `[4,2]`.

#### where bindings vs let bindings


### IO Actions

An IO Action has the type `IO t`.

Actions can be created, assigned, and passed anywhere. However, they may only be performed (executed) from within another I/O action

main itself is an I/O action with type `IO ()`

Performing (executing) an action of type `IO t` may perform I/O and will ultimately deliver a result of type `t`

### File IO

`import System.IO` is necessary.

`Handle` is the core class that represents file handle.
`openFile :: FilePathString -> IOMode -> IO Handle` i.e. returns handle given file path and IO Mode.
`hGetLine :: Handle -> IO String`.
`hGetContents :: Handle -> IO String`.
`hIsEOF :: Handle -> IO Bool`.
Always close your file handles with `hClose`
```hs
        fHandle <- openFile "testdata.txt" ReadMode
        ln <- hGetLine fHandle
        putStrLn ln
        hClose fHandle

        -- get all content in file using hGetContents
        fH2 <- openFile "good_data.txt" ReadMode
        allContent <- hGetContents fH2
        putStrLn allContent
        hClose fH2
```

Write to handle is done via `hPutStrLn`
`hPutStrLn :: Handle -> String -> IO ()`

In Haskell, `return` is the opposite of `<-`. That is, return takes a pure value and wraps it inside `IO`.

`return` is used to wrap data in a monad. When speaking about I/O, `return` is used to take pure data and bring it into the IO monad

There are three well known pre defined handles in `System.IO`:
```hs
stdin :: Handle
stdout :: Handle
stderr :: Handle
getLine = hGetLine stdin
putStrLn = hPutStrLn stdout
print = hPrint stdout
```

### Program to combine contents of files in current directory

```hs
import System.IO
import System.Directory
import Data.Text

getFileContents :: String -> IO String
getFileContents fileName = do
  case fileName of
    -- ignoring directories
    "." -> return ("")
    ".." -> return ("..")
    ".ghc" -> return (".ghc")
    -- for regular files
    otherwise -> do
      fH <- openFile fileName ReadMode
      contents <- hGetContents fH
      let copycontent = contents
      return (copycontent)

main = do
        putStrLn "Greetings! what is your name?"
        inpStr <- getLine
        putStrLn $ "Welcome to haskell, " ++ inpStr ++ "!"
        
        dirs <- getDirectoryContents "."
        mapM_ putStrLn dirs
        let allContentIO = (Prelude.map getFileContents dirs) in
          do
            let allContent = (sequence allContentIO) in
              do
                allContentString <- allContent
                mapM_ putStrLn allContentString
```

### sequence is usefule when dealing with monad arrays

```hs
sequence  :: Monad m => [m a] -> m [a]
```

