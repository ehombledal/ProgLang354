module SkelNumbers where

-- Haskell module generated by the BNF converter

import AbsNumbers
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transExp :: Exp -> Result
transExp x = case x of
  Plus exp1 exp2 -> failure x
  Sub exp1 exp2 -> failure x
  Times exp1 exp2 -> failure x
  Divide exp1 exp2 -> failure x
  Num integer -> failure x

