{-# LANGUAGE InstanceSigs #-}
module StateExample where

newtype Moi s a = Moi { runMoi :: s -> (a, s)}

-- functor
instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi sasfn) = Moi $ \s -> let (a, s') = sasfn s
                                   in (f a, s')
                                         
-- applicative instance
instance Applicative (Moi s) where

    pure :: a -> Moi s a
    pure x = Moi $ \s -> (x, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi sasx) <*> (Moi sasy) = Moi $ \s -> let (ab, s') = sasx s
                                            in
                                                let (a, s'') = sasy s'
                                                in (ab a, s'') 

instance Monad (Moi s) where
    (>>=) (Moi sas) fn = Moi (\s0 -> let (a, s1) = sas s0
                                     in
                                         let (Moi sbs) = fn a
                                         in sbs s1)

main = do
    putStrLn "hello state xample"