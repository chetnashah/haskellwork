module Chap17 where

import Data.Monoid
import Control.Applicative

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity f) (Identity x) = Identity (f x)  

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

jj = Constant { getConstant = 22} :: (Num a) => Constant a String

instance Functor (Constant a) where
    fmap f (Constant x) = Constant { getConstant = x}

instance Monoid a => Applicative (Constant a) where
    pure x  = Constant { getConstant = mempty}
    (<*>) (Constant f) (Constant x) = Constant (x)

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- construct cow if everything is Just
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty -> Just (Cow nammy agey weighty)

cowFromString' name' age' weight' =
    Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')



main = do
    putStrLn "hi"
