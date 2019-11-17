{-# LANGUAGE InstanceSigs #-}
module Chap25 where

import Control.Applicative
import Data.Foldable

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

-- foldMap :: Monoid m => (a -> m) -> t a -> m
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap tr (Compose fga) = foldMap (\ga -> foldMap tr ga) fga


    -- let uu = Compose $ Just [1,2,3]
    -- :t uu = Compose Maybe [] a
    -- try out foldMap Sum uu 
    -- Sum {getSum = 6}

-- foldMap, fmap available.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative f1 => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
    -- (a -> f1 b) -> g a -> f1 (g b)
    -- (a -> f1 b) -> f a -> f1 (f b)
    traverse fn (Compose fga) = let x = traverse (\ga -> traverse fn ga) fga in Compose <$> x


newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

-- The functor and applicative instances are similar to
-- that of Identity, only thing thats changed is extra stuff
-- for structure f
instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad k) => Monad (IdentityT k) where
    return = pure
    (>>=) :: IdentityT k a -> (a -> IdentityT k b) -> IdentityT k b
    (>>=) (IdentityT ka) fn = IdentityT $ ka >>= \a -> runIdentityT (fn a) 

main :: IO ()
main = do
    putStrLn "hello!"