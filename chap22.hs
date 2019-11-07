{-# LANGUAGE InstanceSigs #-}
module Chap22 where
    
import Control.Applicative


boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop -- also runs boop after doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName, dogName :: DogName, address:: Address } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName,
    dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Bill") (DogName "puppy") (Address "Park Avenue")

chris :: Person
chris = Person (HumanName "Chriss") (DogName "doggo") (Address "Main Avenue")

getDog :: Person -> Dog
getDog p = Dog { dogsName = dogName p, dogsAddress = address p }

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
-- Here is how we deduce the types
-- type of dogName : Reader Person DogName or you can say K DogName
-- type of address : Reader Person Address or you can say K Address
-- type of Dog <$> dogName <*> address is
-- (DogName -> Address -> Dog) <$> (K DogName) <*> (K Address)
-- K Dog
-- Reader Person Dog
-- Person -> Dog

-- or you can notice the liftA2 signature that is
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- (DogName -> Address -> Dog) -> (Reader Person DogName) -> (Reader Person Address) -> (Reader Person Dog)
getDogR' = liftA2 Dog dogName address

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader (const x)

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra)  = Reader (\r -> rab r (ra r))

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> ???

main :: IO ()
main = do
    putStrLn "Hi chap 22"
    putStrLn $ show $ bip 11 