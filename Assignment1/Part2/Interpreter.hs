module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) + (eval m)
eval (Times n m) = (eval n) * (eval m)
eval (Sub n m) = (eval n) - (eval m)
eval (Divide n m) = (eval n) / (eval m)
eval (Exponent n m) = (eval n) ^ (eval m)
eval (Modulus n m) = (eval n) % (eval m)