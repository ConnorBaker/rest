{-# LANGUAGE OverloadedStrings #-}

module Test.Orphan.Group where

import Test.Lib.DSL (v, x, y, zero', (#+), (~>))
import qualified Data.HashSet as S
import Language.REST.Internal.Rewrite (Rewrite)
import Language.REST.MetaTerm (MetaTerm (RWApp))
import Language.REST.Op (Op (Op))

neg :: MetaTerm -> MetaTerm
neg x1 = RWApp (Op "neg") [x1]

evalRWs :: S.HashSet Rewrite
evalRWs = S.empty

userRWs :: S.HashSet Rewrite
userRWs =
    S.fromList
      [
          x #+ zero'    ~> x
        , zero'    #+ x ~> x
        , neg x #+ x  ~> zero'
        , (x #+ y) #+ v ~> x #+ (y #+ v)
      ]
