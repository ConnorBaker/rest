{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Completion where

import Control.Monad.Identity ()
import Data.Hashable (Hashable)

import Test.Lib.DSL ((~>))
import qualified Data.HashSet as S
import Language.REST.Internal.OpOrdering ()
import Language.REST.MetaTerm as MT (MetaTerm (RWApp))
import Language.REST.OCAlgebra (OCAlgebra)
import Language.REST.OCToAbstract ()
import Language.REST.Op (Op (Op))
import Language.REST.RPO ()
import Language.REST.RuntimeTerm (RuntimeTerm (..))
import Test.Lib.Language.REST.RuntimeTermHelpers (diverges, proveEQ, eval)
import Language.REST.SMT ()
import Language.REST.WQOConstraints as OC ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> TestTree
tests impl =
    testGroup
        "Completion Tests"
        [ testCase "CompleteDiverges" $ diverges impl [App start [], App mid [], App finish []] >>= (@?= False)
        , testCase "Complete1" $ eq (App start []) (App finish []) >>= (@?= True)
        , testCase "EvalComplete2" $ eval completeUserRWs (App start' [App s1 []]) >>= (@?= True) . (== App finish [])
        , testCase "Complete2" $ eq (App start' [App s1 []]) (App finish []) >>= (@?= True)
        ]
  where
    completeUserRWs =
        S.fromList
            [ MT.RWApp start [] ~> MT.RWApp mid []
            , MT.RWApp mid [] ~> MT.RWApp finish []
            , MT.RWApp start' [MT.RWApp s2 []] ~> MT.RWApp mid' [MT.RWApp s1 []]
            , MT.RWApp s1 [] ~> MT.RWApp s2 []
            , MT.RWApp mid' [MT.RWApp s2 []] ~> MT.RWApp finish []
            ]

    eq :: RuntimeTerm -> RuntimeTerm -> IO Bool
    eq = proveEQ impl S.empty completeUserRWs

    start = Op "start"
    mid = Op "mid"
    finish = Op "finish"

    start' = Op "start'"
    mid' = Op "mid'"

    s1 = Op "s1"
    s2 = Op "s2"