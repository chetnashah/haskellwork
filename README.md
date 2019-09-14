
### REPL

ghci is repl for haskell

* use `:t` variable to get type
* use `:k` variable to get kind
* use `:i` binding to get info on binding
* use `:l` filename to load `.hs` file into repl
* To return to the Prelude> prompt,
use the command `:m`, which is short for :module. This will unload the
file from GHCi, so the code in that file will no longer be in scope in
your REPL

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

We need to use `let` in order to declare variables in REPL, but it is not required in file.

### Stack project

Needs to have a `Main.hs` module with a main declaration, which will be entry point for the project/app

`Note`: you can load modules in `gchi` optionally without having a main block.

### Effects in haskell/fp

1. printing/rendering
2. Network requests
3. Hardware events


### Prefix and infix functions

All functions are prefix functions by default and calling syntax is
`fn arg1 arg2`. To use a prefix function as an infix form, you  put it in back quotes e.g . `fn arg1 arg2` is same as ``arg1 `fn` arg2``

To use an infix function, you put it at first position with paranthenses
e.g. `(+) 2 6` is same as `2 + 6`

## Sectioning

Sectioning is a concise way to partially apply arguments to infix operators.

```hs
(++ "ing") "sleep"
(++ "ing") "walk"
```

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

### Evaluation orders in lambda calculus

1. Normal order: Evaluate leftmost, outermost lambdas first, there is a possibility to apply functions before evaluating arguments. similar to call by name but call by name does not evaluate inside the body of an unapplied function.

2. Applicative order: Evaluate leftmost, innermost lambdas first, i.e. strategy in which the arguments of a function are evaluated from left to right in a post-order traversal of reducible expressions (redexes). Also known as call by value.

3. Lazy evaluation: Call by need is a memoized variant of call by name where, if the function argument is evaluated, that value is stored for subsequent uses.


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

### User defined data types

There are three main ways:
1. Algebraic data types, using `data` keyword
2. Type synonym declarations using `type` keyword
3. Datatype renamings using `newtype` keyword
4. Generalized Algebraic data types `GADTs` with `data + where` keywords

#### Algebraic data types uisng `data`

```
topdecl	→	data [context =>] simpletype [= constrs] [deriving]
```
The parts in brackets are optional
An example is
```hs
  data Maybe a = Just a | Nothing
```
An example with context/constraint is 
```hs
  data Eq a => Set a = NilSet | ConsSet a (Set a)
```
The definition also involves constructors
```
simpletype	→	tycon tyvar1 … tyvark	    (k ≥ 0) // Maybe is a type constructor and a is a typevar in above defition
 
constrs	→	constr1 | … | constrn	    (n ≥ 1)     // data constructors always appear after equals "=" e.g. Just, Nothing

constr	→	con [!] atype1 … [!] atypek	    (arity con  =  k, k ≥ 0)
        |	(btype | ! atype) conop (btype | ! atype)	    (infix conop)
        |	con { fielddecl1 , … , fielddecln }	    (n ≥ 0)

fielddecl	→	vars :: (type | ! atype)
 
deriving	→	deriving (dclass | (dclass1, … , dclassn))	    (n ≥ 0)

```

An algebraic datatype declaration has the form: `data cx => T u1 … uk = K1 t11 … t1k1 | ⋅⋅⋅ | Kn tn1 … tnkn` where `cx` is a context. This declaration introduces a new `type constructor T` with zero or more constituent `data constructors K1, …, Kn`.

Here `tij` could be `type variables` or `type constants`.

The type variables `u1 through uk` must be distinct and may appear in `cx` and the `tij`; it is a static error for any other type variable to appear in `cx` or on the right-hand-side.

Another example:
```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
```

`Field Labels`: 
For large datatypes it is useful to assign field labels to the components of a data object. This allows a specific field to be referenced independently of its location within the constructor.

The arguments to the positional constructor occur in the same order as the labeled fields. For example, the declaration
```hs
  data C = F { f1,f2 :: Int, f3 :: Bool }
```
defines a type and constructor identical to the one produced by
```hs
  data C = F Int Int Bool
```

You can now access values with labels as functions e..g
```
k = F 2 4 True
(f1 k) // prints 2
(f2 k) // prints 4
(f3 k) // prints True
:t k // prints C
```
```hs
-- define data type using records
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- create values with Constructors with records with label"="value
ghci> Car {company="Ford", model="Mustang", year=1967}

-- destructuring with record/labels
tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

Field names share the top level namespace with ordinary variables and class methods and must not conflict with other top level names in scope.

The pattern `F {}` matches any value built with constructor `F`, whether or not `F` was declared with record syntax.

#### Type synonym declarations via `type`

```
type tycon tyvar1 .. tyvark = existingtype
```

It has the form type `T u1 … uk = t` which introduces a new type constructor, `T` . The type `(T t1 …tk)` is equivalent to the type `t[t1∕u1, …, tk∕uk]`. The type variables `u1 through uk` must be distinct and are scoped only over t; it is a static error for any other type variable to appear in t. The kind of the new type constructor `T` is of the form `κ1 →… → κk → κ` where the kinds `κi` of the arguments `ui` and `κ` of the right hand side `t` are determined by kind inference.

Recursive type synonyms are not allowed i.e.
```hs
  type Rec a   =  [Circ a]        -- invalid  
  type Circ a  =  [Rec a]         -- invalid
```

#### Datatype renamings via `newtype`

When using newtype, you're restricted to just one constructor with one field.(See `N` and `t` below)

A declaration of the form `newtype cx => T u1 … uk = N t` introduces a new type whose representation is the same as an existing type. The type `(T u1… uk)` renames the datatype `t`.

YOu can think of `N` as NewConstructor.
The constructor `N` in an expression coerces a value from type `t` to type `(T u1 … uk)`. 
Using `N` in a pattern coerces a value from type `(T u1 … uk)` to type `t`.

Unlike algebraic datatypes, the newtype constructor `N` is unlifted, so that `N ⊥` is the same as `⊥`.



The following examples clarify the differences between data (algebraic datatypes), type (type synonyms), and newtype (renaming types.) Given the declarations
```hs
  data D1 = D1 Int  
  data D2 = D2 !Int  
  type S = Int  
  newtype N = N Int  
  d1 (D1 i) = 42  
  d2 (D2 i) = 42  
  s i = 42  
  n (N i) = 42
```
the expressions `(d1 ⊥)`, `(d2 ⊥)` and `(d2 (D2 ⊥))` are all equivalent to `⊥`, whereas `(n ⊥)`, `(n (N ⊥))`, `(d1 (D1 ⊥))` and `(s ⊥)` are all equivalent to `42`. In particular, `(N ⊥)` is equivalent to `⊥` while `(D1 ⊥)` is not equivalent to `⊥`.

Both newtype and the single-constructor data introduce a single value constructor, but the value constructor introduced by newtype is strict and the value constructor introduced by data is lazy. So if you have
```hs
data D = D Int
newtype N = N Int
```hs
Then `N undefined` is equivalent to `undefined` and causes an error when evaluated. But `D undefined` is not equivalent to undefined, and it can be evaluated as long as you don't try to peek inside.



A newtype declaration may use field-naming syntax, though of course there may only be one field. Thus:
```hs
  newtype Age = Age { unAge :: Int }
```
brings into scope both a constructor and a de-constructor:
```hs
  Age   :: Int -> Age  
  unAge :: Age -> Int
```

#### Generalized algebraic data types GADTs


```hs
data Maybe a where
   Nothing  :: Maybe a
   Just :: a -> Maybe a
```
This syntax is made available by the language option `{-#LANGUAGE GADTs #-}`. It should be familiar to you in that it closely resembles the syntax of type class declarations. It's also easy to remember if you already like to think of constructors as just being functions. Each constructor is just defined by a type signature.



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

Functions in Haskell default to prefix syntax, meaning that the function being applied is at the beginning of the expression rather than
the middle.

An operator is a function that can be applied using infix syntax, or partially applied using a section (Section 3.5).

One can use a prefix function in infix style by wrapping it in backticks. And one can use an infix operator in prefix style by wrapping it in parentheses.

Know more about operators using `:i`, e.g. `:i (*)`
The `infixl` in the output signifies that it is an infix operator and left associative

An operator is either an operator symbol, such as + or $$, or is an ordinary identifier enclosed in grave accents (backquotes), such as ` op `. For example, instead of writing the prefix application op x y, one can write the infix application x` op ` y. If no fixity declaration is given for ` op ` then it defaults to highest precedence and left associativity 

Dually, an operator symbol can be converted to an ordinary identifier by enclosing it in parentheses. For example, `(+) x y` is equivalent to `x + y`, and `foldr (⋆) 1 xs` is equivalent to `foldr (\x y -> x⋆y) 1 xs`.


### Tuples

Combines multiple values into a single value.
Same syntax is the type constructor as well as data constructor: `(,)`

`(,)` is a data constructor for tuples.

`(,) 8 "hi"` generates a tuple `(8, "hi")`

One cannot partially applied variable binding using `(,)`,
E.g. `(,) 1` will throw error.

```hs
data (,) a b = (,) a b -- as there are two different typevars, tuple can be made of two different concrete-types 
fst :: (a, b) -> a
snd :: (a, b) -> b

-- multi tuple constructors are weird!!
(, , ,) 1 2 3 4
-- output: (1, 2, 3, 4)
```

Utilities present in `Data.Tuple`

### Lists

Define as:
```hs
data [] a = [] | a : [a] -- involves cons constructor (:)
```
From the type definition, restriction on the typevar is list
allows only elements of single parametric type `a`

Note: We can see `[]` in both terms and types, i.e. it acts as both
a data constructor and type constructor respectively.

Referencing `length` on a list is generally bad idea being `O(n)`
as it is traversing whole list to give answer.

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

### Numerics

When we query the types of numeric values, we see typeclass
information instead of a concrete type, because the compiler doesn’t
know which specific numeric type a value is until the type is either
declared or the compiler is forced to infer a specific type based on
the function.

Data Types: `Int`, `Float`, `Double`, `Rational` are data types where as

Four sorts of nubmer: `Integral`, `RealFloat`, `Ratio`, `Complex`. This is not finely demarcated though. It is for our understanding.  `Integral` and `RealFloat` are type classes.

`Typeclasses`: `Real`, `Num`, `Integral`, `Fractional` are type classes

Operators under `Num`: `(+)`, `(-)`, `(*)`, Members are: `Integer, Int, Float, Double`. Note how fractional division `(/)` is not a part of `Num`, since it may not satisfy `a -> a -> a` required by `Num`. See `Fractional`.

Operators under `Integral`: `quot`, `div`, `mod`, `rem`, `toInteger`, Members of this typcleassSs: `Int, Integer`.

Operators under `RealFloat`: `floatRadix`, `floatDigits`, `floatRange`, `decodeFloat`, `encodeFloat`, `exponent`, `significand`, `scaleFloat`, `isNaN`, `isInfinite`, `isDenormalized`, `isNegativeZero`, `isIEEE`, `atan2`. Members of RealFloat are `Float`, `Double`.
`RealFloat` extends upon `RealFrac` and `Floating`

Operators under `Real`: `toRational`, Members: `Integer, Int, Float, Double`

Operators under `Fractional` -> `(/)`, `recip`, `fromRational`. Members are: `Float`, `Double`. Since `Int` is not a member of `Fractional` type class, `/` operator does not work on `Int`. One has to do something before it.

Operators under `RealFrac` : `properFraction`, `truncate`, `round`, `ceiling`, `floor`. Instances/members of `RealFrac` are `Float` and `Double`. Also `RealFrac` extends upon `Real` and `Fractional`.

Operators under `Floating`: `pi`, `exp`, `log`, `sqrt`, `**`, `sin,cos,tan,etc`. Instances/members of floating are `Float` and `Double`.

`Ratio`: It is made of Integral number pair
`data (Integral a) => Ratio a = a % a`, Also `type Rational = Ratio Integer`. Import can be done via `Data.Ratio`.

`Complex`: Made of RealFloat number pair

Defaulting: "I need a type (a) with (Num a => a) and (Fractional a => a). (Fractional a) requires (Num a) already, so that part is redundant and I can proceed with just the (Fractional a)". it picks a reasonable instance of Fractional via defaulting, which gives it Double.


![Numeric type class hierarchy](./images/typeclasshierarchy.png)

https://stackoverflow.com/questions/42820603/why-can-a-num-act-like-a-fractional

#### Integer vs Int?

`Integer` is for BigIntegers i.e. it can hold arbitrary large integers in the sense of natural numbers.

`Int` is bounded by Int64 i.e. range `-9223372036854775808..9223372036854775807`

Other integers with smaller ranges are `Int8`, `Int16`, `Int32` and `Int64` which are availble under importing `GHC.Int`. and are also members of `Bounded` type class.

### Bindings

Using `let` and `where` and `pattern matching` by defining functions,
We introduced `bindings`. Bindings are essentially names for expressions.
And cannot be rebound in same scope.

### Reverse thin arrow operator `<-`

In case of List comprehensions and case guards,
`<-` means a pattern match.

In case of mondadic values, `<-` means run an action or thing of it as unwrapping a monad value,
equivalent to `>>=` in a normal expression.

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

**let without `in`**: introduces binding in the current scope. Also known as `let-statement`.
e.g.
```hs
do statements
    let variable = exp -- variable binding available inside do block
    statements
```

### Let vs Where

Let and Where are fundamentally different.

`Let introduces an expression` hence we say `let expression` can be used wherever an expression is expected.

`Where is a declaration` that bounds to a surrounding construct.


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

A list comprehension has the form `[ e | q1, ..., qn ], n>=1`, where the `qi` qualifiers are either

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

