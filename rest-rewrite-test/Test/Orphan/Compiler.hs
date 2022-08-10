{-# LANGUAGE OverloadedStrings #-}

module Test.Orphan.Compiler where

import qualified Test.Lib.Arith as A
import Test.Lib.DSL (suc', x, y, zero', (<~>), (~>))
import qualified Data.HashSet as S
import Language.REST.Internal.Rewrite (Rewrite)
import Language.REST.MetaTerm (MetaTerm (RWApp))
import Language.REST.Op (Op (Op))
import Prelude hiding (repeat, seq)

repeat :: MetaTerm -> MetaTerm -> MetaTerm
repeat n op = RWApp (Op "repeat") [n, op]

seq :: MetaTerm -> MetaTerm -> MetaTerm
seq op1 op2 = RWApp (Op "seq") [op1, op2]

nop :: MetaTerm
nop = RWApp (Op "nop") []

userRWs :: S.HashSet Rewrite
userRWs =
  S.union
    A.userRWs
    ( S.fromList $
        [ seq x nop ~> x
        , seq nop x ~> x
        , repeat zero' x ~> nop
        ]
          ++ (repeat (suc' y) x <~> seq x (repeat y x))
          -- ++ (repeat (suc' y) x <~> seq (repeat y x) x)
          ++ (repeat (suc' (suc' zero')) x <~> seq x x)
    )

evalRWs :: S.HashSet Rewrite
evalRWs = S.empty
