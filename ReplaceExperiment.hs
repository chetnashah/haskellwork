module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'


lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohooo"]


liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe String]-> String
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe String] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted



main = do
    putStrLn "jello"

    putStr "replaceWithP' lms: "
    putStr "liftedReplace lms: "
    print (liftedReplace lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted lms: "
    print (twiceLifted lms)
    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)
    putStr "thriceLifted lms: "
    print (thriceLifted lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)
