A Haskell source file contains a definition of a single module.

A source file begins with a module declaration. This must precede all other definitions in the source file.

**Note** - A module name has to match file name. filename can be lowercase but module name has to be uppercase.

Modules contain datatypes, type synonyms, typeclasses, typeclass instances and values
defined at the top level.

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

`Imports`: when you do a regular import of a module,
All the names inside the module are bound with same name in current module (Without any name spacing)

When you have multiple bindings of same name, one due to import and one due to binding in current module,
one gets error like:
```
    Ambiguous occurrence `dogs'
    It could refer to either `Main.dogs', defined at exe\Main.hs:6:1
                          or `DogsRule.dogs',
```

All of your imports must occur after the module has
been declared and before any expressions have been defined in your
module.
e.g.
```hs
-- bad code
module Addition where
sayHello :: IO ()
sayHello = putStrLn "hello!"
import Test.Hspec -- invalid!!!!, move this before sayHello
```

Note: Ordering of import declarations is irrelevant.

#### Selective imports

```hs
-- only import into scope the names mask and try
import Control.Exception (mask, try)
import Control.Monad (forever, when)
```

#### Qualified vs unqualified imports

Here is an example of qualified imports:
```hs
import qualified Data.Bool

-- usage needs full namespace
Data.Bool.not True == False
```

Qualified import with alias, useful since namespaces get quite big
```hs
import qualified Data.Bool as B
B.not True == False
```

### Browsing modules in GHCi

Use `:browse ModuleName` in ghci, to see the declarations in the module.



### Cabal

Cabal exists primarily to describe a single package with a `.cabal` file.
Stack is built on top of cabal.

A package is a program you are building includng all of its modules and dependencies.

#### `.cabal` file

1. `executable stanza` - make a cli app that can be run
e.g.
```cabal
executable hello
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5, libnamethisexecutabledependson
```

2. `library stanza` - package code for reuse.
```cabal
library
  hs-source-dirs:      src
  exposed-modules:     Hello
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```

### Stack

`stack new`: will create a new directory containing all the needed files to start a project correctly

`stack setup`: download compiler if necessary.

`stack build`: run a build given config, usually followed by `stack setup`

`stack path`: list all paths used by the project

`stack exec binName`: execute given binary

`stack install pkgname`: install an executable using stack.

Stack reads stanzas in `.cabal` files to decide what artifacts to be built.



### Build system

Stack relies on an LTS snapshot of haskell packages from stackage "www.stackage.org" that are guaranteed to work well togehter.

https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/

