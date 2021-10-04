-- A Virtual Machine (VM) for Arithmetic (template)

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Eq,Show) -- for equality and printing

-- Rational numbers
data QQ =  QQ II PP
  deriving (Eq,Show)

------------------------
-- Arithmetic on the  VM
------------------------

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

----------------
-- II Arithmetic
----------------
-- Addition: (a-b) + (c-d) = (a+c)-(b+d)
addI :: II -> II -> II
addI (II a b) (II c d) = II (addN a c) (addN b d)

-- Multiplication: (a-b)*(c-d)=(ac+bd)-(ad+bc)
multI :: II -> II -> II
multI (II a b) (II c d) = II (addN (multN a c) (multN b d)) (addN (multN a d) (multN b c))

-- Subtraction: (a-b)-(c-d)=(a+d)-(b+c)
subtrI :: II -> II -> II
subtrI (II a b) (II c d) = II (addN a d) (addN b c)

-- Negation: -(a-b)=(b-a)
negI :: II -> II
negI (II a b) = (II b a)

----------------
-- QQ Arithmetic
----------------

--add positive numbers
addP :: PP -> PP -> PP
addP I m = T m 
addP (T p) x = T (addP p x)

--multiply positive numbers
multP :: PP -> PP -> PP 
multP I m = m
multP (T p) x = addP (multP (p) x) x

--convert numbers of type PP to numbers of type II 
ii_pp:: PP -> II 
ii_pp I = II (S O) O 
ii_pp (T n) = addI (ii_pp n) (ii_pp I) --done in office hours 

 --Addition (a/b) + (c/d) = (ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (addI (multI (a) (ii_pp(d))) (multI (c) (ii_pp(b)))) (multP (b) (d))


 --Multiplication (a/b)*(c/d) = (ac)/(bd)
multQ :: QQ -> QQ -> QQ  
multQ (QQ a b) (QQ c d) = QQ (multI a c) (multP b d)

----------------
-- Normalisation
----------------
----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------
-- Precondition: Inputs are non-negative
nn_int :: Integer -> NN
nn_int 0 = O
nn_int n = S(nn_int(n - 1))

int_nn :: NN->Integer
int_nn O = 0
int_nn (S n) = 1 + int_nn n

ii_int :: Integer -> II
ii_int 0 = II O O
ii_int n = II (nn_int n) O

int_ii :: II -> Integer
int_ii (II O O) = 0
int_ii (II n m) = int_nn n - int_nn m

-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int 1 = I
pp_int n = T(pp_int (n - 1))

int_pp :: PP->Integer
int_pp I = 1
int_pp (T n) = 1 + int_pp n

float_qq :: QQ -> Float
float_qq (QQ a b) = fromIntegral (int_ii (a)) / fromIntegral (int_pp(b))


----------
-- Testing
----------
main = do
    let i = 4
    let j = 2
    let k = 1
    let l = 3

    print $ float_qq (addQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    print $ float_qq (multQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))

