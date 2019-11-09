module LearnParsers where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

oneEof = one >> eof
oneTwoEof = oneTwo >> eof

oneString :: Parser String
oneString = string "1"

oneTwoString :: Parser String
oneTwoString = string "12"

testParseString :: Parser String -> IO ()
testParseString p = print $ parseString p mempty "123"

p123 :: String -> IO ()
p123 s = print $ parseString p mempty s
  where
    p = (try (string "123") <|> try (string "12") <|> try (string "1")) <* eof


main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'