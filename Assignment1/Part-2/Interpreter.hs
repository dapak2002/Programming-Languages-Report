module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) + (eval m)
eval (Sub n m) = (eval n) - (eval m)
eval (Times n m) = (eval n) * (eval m)
eval (Div n m) = (eval n) 'quot' (eval m)
eval (Mod n m) = (eval n) `mod` (eval m)
eval (Expn n m) = (eval n) ^ (eval m)
eval (Abs n) = abs (eval n)
eval (Neg n) = (eval n) * (-1)