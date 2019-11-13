{-# LANGUAGE InstanceSigs #-}
module Chap25 where

import Control.Applicative

newtype Compose f g a = Compose { getCompose :: f (g a) }
                        deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ pure (pure x)

    (<*>) :: Compose f g (p -> q) -> Compose f g p -> Compose f g q
    (<*>) (Compose fgx) (Compose fgy) = Compose $ liftA2 (<*>) fgx fgy

    -- or
    -- (<*>) (Compose fgx) (Compose fgy) = Compose $ (<*>) <$> fgx <*> fgy
main :: IO ()
main = do
    putStrLn "hello!"