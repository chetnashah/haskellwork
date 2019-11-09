
Trifecta is one of the libraries for parsing.
Other libraries are `parsec`, `attoparsec`, `megaparsec`, `aeson`, `cassava`.

For `Trifecta`: Two libraries: `parsers` and `trifecta` - `trifecta` provides concrete impl of Trifecta Parser but `parsers` API is collection of tpeclasses that abstract over different kinds of things parsers do.

`ByteString` is a type typically used

simple type definition:
```hs
-- consume a string to get a possible value
-- along with remainder string
type Parser a = String -> Maybe (a, String)
-- if you squint a little, we can realize that it closely resembles State typedef.

-- a different one
newtype Parser a = P ([Token] -> [(a, [Token])])
-- Same thing, differently formatted:
type Parser' a = String -> [(a, String)]
```

We use parsers like values and combine them using the same stuff we use with
ordinary functions or operators from Applicative and Monad typeclass.


Parser combinator: A parser combinator combines two or more parsers to produce
a new parser. Good examples of this are things like using `<|>`
from Alternative to produce a new parser from the disjunction
of two parser arguments to `<|>`. Or `some`. Or `many`. Or `mappend`. Or
  `(>>)`


`parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a`

`instance CharParsing Parser`
`char :: CharParsing m => Char -> m Char` so you can say `char` returns a `Parser` for `Char.

`decimal` used for parsing:
`decimal :: TokenParsing m => m Integer`
also `TokenParsing` is instance of `Parser`, so
`decimal` returns a parserof type integer

`integer` used for integer parsing
`integer :: TokenParsing m => m Integer`

### Parsing Typeclass

Useful methods under `Parsing` typeclass are : 

`Parsing` is superclass of other typeclass like `CharParsing`.
```hs
class GHC.Base.Alternative m => Parsing (m :: * -> *) where
  -- consume input and fail if cannot
  try :: m a -> m a
  (<?>) :: m a -> String -> m a
  skipMany :: m a -> m ()
  skipSome :: m a -> m ()
  -- emit error on unexpected token
  unexpected :: String -> m a
  default unexpected :: (Control.Monad.Trans.Class.MonadTrans t,
                         Monad n, Parsing n, m ~ t n) =>
                        String -> m a
  -- succeed on end of input
  eof :: m ()
  default eof :: (Control.Monad.Trans.Class.MonadTrans t, Monad n,
                  Parsing n, m ~ t n) =>
                 m ()
  notFollowedBy :: Show a => m a -> m ()
  {-# MINIMAL try, (<?>), notFollowedBy #-}
        -- Defined in `Text.Parser.Combinators'
instance Parsing m => Parsing (Unspaced m)
  -- Defined in `Text.Parser.Token'
instance Parsing m => Parsing (Unlined m)
  -- Defined in `Text.Parser.Token'
instance Parsing m => Parsing (Unhighlighted m)
  -- Defined in `Text.Parser.Token'

-- Parsing is an instance of Parser!!
instance Parsing Parser -- Defined in `Text.Trifecta.Parser'
```


```hs
-- Parses a sequence of characters, returns
-- the string parsed.
string :: String -> m String
-- Parses a sequence of characters represented
-- by a Text value, returns the parsed Text fragment.
text :: Text -> m Text

-- Parser succeeds for any character.
-- Returns the character parsed.
anyChar :: m Char
```