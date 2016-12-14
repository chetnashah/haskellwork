

data MyBool = MFalse | MTrue deriving Show


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
x <=> y = x == y

-- logical xor , returns tru only when both are different
(<+>) :: MyBool -> MyBool -> MyBool
x <+> y = x /= y -- bet you didn't know the not equal to operator right?

-- logical validity and checking truth tables, using list comprehensions