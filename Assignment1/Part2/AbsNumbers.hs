

module AbsNumbers where

-- Haskell module generated by the BNF converter




data Exp
    = Plus Exp Exp
    | Sub Exp Exp
    | Times Exp Exp
    | Divide Exp Exp
    | Exponent Exp Exp
    | Num Integer
  deriving (Eq, Ord, Show, Read)

