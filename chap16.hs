module Chap16 where


data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f1 (Matter a) = Matter (f1 a)

    

main = do
    putStrLn "Hello world!1"