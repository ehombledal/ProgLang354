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
negI (II a b) = (II b a) --covered in section 1 lecture! 

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
{- ii_pp:: PP -> II 
ii_pp I = II (S O) --covered in section 2 lecture 
ii_pp (T n) = addI(SO) n --expected type II, actual type PP 
 -}
----------------
-- Normalisation
----------------


----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------


----------
-- Testing
----------
main = do

    --print $ addN (S (S O)) (S O)
    --print $ multN (S (S O)) (S (S (S O)))

    --print $ addI (II (S (S O)) (S (S (S O)))) (II (S O) (S (S O)))
    --print $ multI (II (S (S O)) (S (S (S O)))) (II (S O) (S (S O)))
    --print $ subtrI (II (S (S O)) (S (S (S O)))) (II (S O) (S (S O)))

    --print $ addP (T (T I)) (T I)
    --print $ multP (T I) (T I)

    print $ negI (II (S(S O)) (S O))


