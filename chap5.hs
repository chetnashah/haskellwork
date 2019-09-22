module Chap5 where

    data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    data Date = Date DayOfWeek Int

    instance Eq DayOfWeek where
        (==) Mon Mon = True
        (==) Tue Tue = True
        (==) Wed Wed = True
        (==) Thu Thu = True
        (==) Fri Fri = True
        (==) Sat Sat = True
        (==) Sun Sun = True
        (==) _ _ = False
    
    instance Eq Date where
        (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
            weekday == weekday' && dayOfMonth == dayOfMonth'
    
    data Identity a = Identity a

    instance Eq a => Eq (Identity a) where
        (==) (Identity v) (Identity v') = v == v'

    data Mood = Blah | Woot deriving (Eq, Show)

    settleDown x = if x == Woot then Blah else x

    type Subject = String
    type Verb = String
    type Object = String

    data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

    s1 = Sentence "dogs" "drool"
    s2 = Sentence "julie" "loves" "dogs"

    data Rocks = Rocks String deriving (Eq, Show)
    data Yeah = Yeah Bool deriving (Eq, Show)

    data Papu = Papu Rocks Yeah deriving (Eq, Show)

    equalityForall :: Papu -> Papu -> Bool
    equalityForall p p' = p == p'

    f :: Float
    f = 1.0

    freud :: a -> a
    freud x = x

    test :: String 
    test = "Qrew" :: String

    main = do
        print "Hello"


