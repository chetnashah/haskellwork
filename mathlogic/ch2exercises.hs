

data MyBool = MFalse | MTrue deriving (Show, Eq)


mnot :: MyBool -> MyBool
mnot MTrue = MFalse
mnot MFalse = MTrue

-- letter operators can be defined by infix using backticks
mAnd :: MyBool -> MyBool -> MyBool
MFalse `mAnd` x = MFalse
MTrue `mAnd` x = x

-- letter infix operator are defined using backticks
mOr :: MyBool -> MyBool -> MyBool
MFalse `mOr` x = x
MTrue  `mOr` x = MTrue

-- if operator is made from any of !#$%&*+./<=>?@\^|-~: one can use (++)
-- kind of thing to define operator and does not need backticks in that case 
-- implication operator
(==>) :: MyBool -> MyBool -> MyBool
x ==> y = (mnot x) `mOr` y

-- alternate definition which is same as above for implication
(===>) :: MyBool -> MyBool -> MyBool
MTrue ===> x = x
MFalse ===> x = MTrue

-- logical equivalence operator, returns true when only both are exactly same
(<=>) :: MyBool -> MyBool -> MyBool
x <=> y | x == y = MTrue
        | otherwise = MFalse

-- logical xor , returns tru only when both are different
(<+>) :: MyBool -> MyBool -> MyBool
x <+> y | x /= y = MTrue -- bet you didn't know the not equal to operator right?
        | otherwise = MFalse


-- logical validity and checking truth tables, using list comprehensions

-- formula with two input boolean variables
frm :: MyBool -> MyBool -> MyBool 
frm p q = p ==> (q ==> p)


valid :: (MyBool -> MyBool) -> MyBool
valid bf = (bf MTrue) `mAnd` (bf MFalse)

-- valid is a higher order function that takes a formula
-- and returns if its valid for all possible input variables
valid2 :: (MyBool -> MyBool -> MyBool) -> MyBool
valid2 bf = (bf MTrue MTrue) `mAnd` (bf MTrue MFalse) `mAnd` (bf MFalse MTrue) `mAnd` (bf MFalse MFalse)

-- try out in GhCi;
-- # valid2 frm

-- logical equivalence is higher order function that takes two formulas
-- and returns if they are logically equivalent
logEquiv2 :: (MyBool -> MyBool -> MyBool) -> (MyBool -> MyBool -> MyBool) -> MyBool
logEquiv2 bf1 bf2 = foldr mAnd MTrue allPossibilities 
        where allPossibilities = [ (bf1 p q) <=> (bf2 p q) | p <- [MTrue, MFalse], q <- [MTrue, MFalse]]


-- two formulas of two variables
frma p q = p
frmb p q = (p <+> q) <+> q

-- try out in GhCi-
-- # logEquiv2 frma frm

-- Note: you can convert checking of two formulas of same number vars to be logically equivalent
-- into a single formula of logical validity checking using equivalence (<=>)
-- e.g.
-- combinded fomula
frmc p q = p <=> (p <+> q) <+> q

-- now you can directly do 
-- valid2 frmc

-- exercise 2.15
-- an opposition to valid that is contradiction
-- a formula that results false for every possible combination of
-- boolean variables is a contradiction, say like Fallacy table
contradiction :: (MyBool -> MyBool) -> MyBool
contradiction bf = ((bf MTrue) `mOr` (bf MFalse)) <=> MFalse

-- contradiction for formulae of two variables
contradiction2 :: (MyBool -> MyBool -> MyBool) -> MyBool
contradiction2 bf = (bf MTrue MTrue) `mOr` (bf MTrue MFalse) `mOr` (bf MFalse MTrue) `mOr` (bf MFalse MFalse)


-- Quantifiers as procedures
-- Well known quantifiers in logic are Universal(all) and Existential(any)

-- a well known predicate, which also acts for set membership
isEven x
  | x `mod` 2 == 0 = MTrue
  | otherwise = MFalse

-- prelude has all and any
-- note first parameter is a predicate (which is equivalent of testing set membership)
mAll :: (a -> MyBool) -> [a] -> MyBool
mAll p = foldr mAnd MTrue . map p

mAny :: (a -> MyBool) -> [a] -> MyBool
mAny p = foldr mOr MFalse . map p 

-- prelude does not have every and some
-- difference is they take list/data first and then the predicate
-- This notation is closer to mathematical notation, i.e. given restricted quantifier, get output of quantified predicate
mEvery :: [a] -> (a -> MyBool) -> MyBool
mEvery xs p = mAll p xs

mSome :: [a] -> (a -> MyBool) -> MyBool
mSome xs p = mAny p xs

-- note, calls to any, all, every, some need not terminate in case of infinite lists
-- e.g. mEvery [0..] (>=0)


-- converts a regular predicate to one which is compatible with MyBool
mConvert :: (a -> Bool) -> a -> MyBool
mConvert pred x
  | pred x = MTrue
  | otherwise = MFalse

main = putStrLn "Hi world"
