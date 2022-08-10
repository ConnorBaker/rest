{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Arithmetic where

import qualified Test.Lib.Arith as A
import Control.Monad.Identity ()
import Test.Lib.DSL (
    ack,
    suc',
    t1,
    t2,
    t3,
    t4,
    x,
    y,
    zero,
    zero',
    (#+),
    (.+),
    (~>),
 )
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Language.REST.ExploredTerms ()
import Language.REST.Internal.OpOrdering ()
import Language.REST.Internal.Rewrite (subst)
import Language.REST.Internal.WorkStrategy ()
import Language.REST.MetaTerm as MT (MetaTerm (RWApp))
import Language.REST.OCAlgebra (OCAlgebra)
import Language.REST.OCToAbstract ()
import Language.REST.Op (Op (Op))
import Language.REST.RPO ()
import Language.REST.Rest ()
import Language.REST.RuntimeTerm (RuntimeTerm (..), contains)
import Test.Lib.Language.REST.RuntimeTermHelpers (diverges, proveEQ, eval)
import Language.REST.SMT ()
import Language.REST.WQOConstraints as OC ()
import Test.Lib.Nat (intToTerm, termToInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> TestTree
tests impl =
    testGroup
        "Arithmetic Tests"
        [ testCase "Contains" $ (@?= True) $ contains (intToTerm 2) (intToTerm 1)
        , testCase "Diverge" $ diverges impl [intToTerm 2 .+ t1, intToTerm 1 .+ t1] >>= (@?= False)
        , testCase "Diverge3" $ diverges impl [(t1 .+ t2) .+ t3, t1 .+ (t2 .+ t3), (t2 .+ t3) .+ t1] >>= (@?= False)
        , testCase "Eval1" $ arithEQ (intToTerm 2 .+ intToTerm 3) 5 >>= (@?= True)
        , testCase "Eval2" $ arithEQ (ack (intToTerm 3) (intToTerm 2)) 29 >>= (@?= True)
        , testCase "Subst1" $ (@?= True) $ subst (M.fromList [("X", intToTerm 1), ("Y", intToTerm 2)]) (x #+ y) == (intToTerm 1 .+ intToTerm 2)
        , testCase "ArithTerm" $ termTest >>= (@?= True)
        , testCase "ArithTerm2" $ termTest2 >>= (@?= True)
        , testCase "Arith0" $ eq (t1 .+ t2 .+ intToTerm 1) (t1 .+ (intToTerm 1 .+ t2)) >>= (@?= True)
        , testCase "Arith1" $ eq (intToTerm 2 .+ intToTerm 3) (intToTerm 3 .+ intToTerm 2) >>= (@?= True)
        , testCase "Arith2" $ eq (t1 .+ t2) (t2 .+ t1) >>= (@?= True)
        , testCase "Arith3" $ eq (t2 .+ t1) (t2 .+ t2) >>= (@?= False)
        , testCase "Arith4" $ eq ((t1 .+ t2) .+ t3) (t1 .+ (t2 .+ t3)) >>= (@?= True)
        , testCase "Arith4.1" $ eq (t1 .+ t2 .+ t3) (t3 .+ t2 .+ t1) >>= (@?= True)
        , testCase "Arith5" $ eq (zero .+ t1) t1 >>= (@?= True)
        , testCase "Arith5.1" $ eq (zero .+ zero .+ t1) t1 >>= (@?= True)
        , testCase "Arith5.2" $ eq (zero .+ zero .+ zero .+ t1) t1 >>= (@?= True)
        , testCase "Arith6" $ eq (((t1 .+ t1) .+ t3) .+ t4) (t1 .+ (t1 .+ (t3 .+ t4))) >>= (@?= True)
        , testCase "Arith7" $ eq ((intToTerm 2 .+ intToTerm 1) .+ t1) (intToTerm 2 .+ (intToTerm 1 .+ t1)) >>= (@?= True)
        ]
  where
    arithEQ t n = do
        t' <- eval A.evalRWs t
        return $ termToInt t' == Just n

    termTest = proveEQ impl evalRWs userRWs (App f1 [t1]) zero
      where
        evalRWs = S.union termEvalRWs A.evalRWs
        userRWs = S.insert (MT.RWApp g1 [x] ~> MT.RWApp f1 [x]) A.userRWs
        termEvalRWs =
            S.fromList
                [ MT.RWApp f1 [x] ~> MT.RWApp g1 [suc' x]
                , MT.RWApp g1 [x] ~> zero'
                ]
        f1 = Op "f"
        g1 = Op "g"

    termTest2 = proveEQ impl evalRWs userRWs (App f1 [zero]) (App g1 [zero])
      where
        evalRWs = S.union termEvalRWs A.evalRWs
        userRWs = S.insert (MT.RWApp f1 [x] ~> MT.RWApp g1 [suc' (suc' x)]) A.userRWs
        termEvalRWs =
            S.fromList
                [ MT.RWApp f1 [suc' x] ~> MT.RWApp g1 [suc' x]
                , MT.RWApp f1 [zero'] ~> zero'
                , MT.RWApp g1 [suc' x] ~> MT.RWApp f1 [x]
                , MT.RWApp g1 [zero'] ~> zero'
                ]
        f1 = Op "f"
        g1 = Op "g"

    eq = proveEQ impl A.evalRWs A.userRWs