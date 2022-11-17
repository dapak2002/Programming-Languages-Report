-- A Virtual Machine (VM) for Arithmetic (specification)
-- Copyright: Alexander Kurz 2022

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Eq,Show) -- for equality and printing

-- Rational numbers
data QQ =  QQ II PP
  deriving (Show) -- for equality and printing

------------------------
-- Arithmetic on the  VM
------------------------

----------------
-- PP Arithmetic
----------------

-- add positive numbers
addP :: PP -> PP -> PP
addP I m = (T m)
addP (T n) m = T (addP n m)


-- multiply positive numbers
multP :: PP -> PP -> PP
multP I m = m
multP (T n) m = addP (multP n m) m

-- convert numbers of type PP to numbers of type NN
nn_pp :: PP -> NN
nn_pp I = (S O)
nn_pp (T n) = S (nn_pp n)

-- convert numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp (T n) = II (nn_pp n) O

----------------
-- NN Arithmetic
----------------

-- add natural numbers
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- multiply natural numbers
multN :: NN -> NN -> NN
multN O m = O
multN (S n) m = addN (multN n m) m

-- subtract natural numbers
subN :: NN -> NN -> NN
subN O m = O
subN m O = m
subN (S n) (S m) = subN n m

-- division, eg 13 divided by 5 is 2 
divN :: NN -> PP -> NN
divN O m = O
divN n m 
  | int_nn n < int_pp m = O
  | otherwise = S(divN (subN n (nn_pp m)) m)

-- remainder, eg 13 modulo by 5 is 3
modN :: NN -> PP -> NN
modN O m = O 
modN n m
  | int_nn n < int_pp m = n
  | otherwise = modN (subN n (nn_pp m)) m

----------------
-- II Arithmetic
----------------

-- Addition: (a-b)+(c-d)=(a+c)-(b+d)
addI :: II -> II -> II
addI (II a b) (II c d) = II (addN a c) (addN b d)

-- Multiplication: (a-b)*(c-d)=(ac+bd)-(ad+bc)
multI :: II -> II -> II
multI (II a b) (II c d) = II (addN (multN a c) (multN b d)) (addN (multN a d) (multN b c))

-- Negation: -(a-b)=(b-a)
negI :: II -> II
negI (II a b) = (II b a)

-- Equality of integers
instance Eq II where
  (II a b) == (II c d) = True
----------------
-- QQ Arithmetic
----------------

-- Addition: (a/b)+(c/d)=(ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (addI (multI a (ii_pp d)) (multI (ii_pp b) c)) (multP b d)

-- Multiplication: (a/b)*(c/d)=(ac)/(bd)
multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ c d) = QQ (multI a c) (multP b d)

-- Equality of fractions
instance Eq QQ where
  (QQ a b) == (QQ c d) = True
----------------
-- Normalisation
----------------

normalizeI :: II -> II
normalizeI (II n O) = (II n O)
normalizeI (II O n) = (II O n)
normalizeI (II (S m) (S n)) = normalizeI (II n m)

----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

-- Precondition: Inputs are non-negative
nn_int :: Integer -> NN
nn_int 0 = O
nn_int n = addN (S O) (nn_int (n - 1))

int_nn :: NN -> Integer
int_nn O = 0
int_nn (S n) = 1 + (int_nn n)

ii_int :: Integer -> II 
ii_int 0 = II O O 
ii_int n 
  | n < 0 = II O (nn_int(-1 * n))
  | n > 0 = II (nn_int(n)) O

int_ii :: II -> Integer
int_ii (II O O) = 0
int_ii (II (n) m) = int_nn(n) - int_nn(m)

-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int 1 = I
pp_int n = addP I (pp_int (n - 1))

int_pp :: PP -> Integer
int_pp I = 1
int_pp (T n) = 1 + int_pp (n)

float_qq :: QQ -> Float
float_qq (QQ (II n m) p) = a / b where
  a = fromIntegral (int_ii (II n m)) :: Float
  b = fromIntegral (int_pp p) :: Float

------------------------------
-- Normalisation by Evaluation
------------------------------

-- nbe :: II -> II
-- nbe n = ii_int(int_ii n))

----------
-- Testing
----------

main = do
    print $ int_nn (multN (nn_int 4) (nn_int 3)) -- 12
    