{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.OpOrdering where

import Test.Lib.DSL (f, g, t1Op, t2Op)
import Data.Hashable (Hashable)
import Data.Maybe as Mb (fromJust)
import Language.REST.Internal.OpOrdering (parseOO, (=.), (>.))
import Language.REST.Internal.WQO (merge, mergeAll)
import Language.REST.Op (Op)
import Language.REST.RPO (rpoGTE)
import Language.REST.WQOConstraints as OC (
  WQOConstraints (intersect, noConstraints, permits),
  isUnsatisfiable,
  singleton,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "OpOrdering Tests"
    [ testCase "Parse" $
        (@?= True) $
          fromJust (parseOO "cons = z ∧ g = nil ∧ h = s ∧ cons > g ∧ cons > h ∧ h > f ∧ h > g") == wqo
    , testCase "Minimal" $
        (@?= True) $
          fromJust (parseOO "f > + ^ g > z ^ + = cons ^ cons > z ^ f = g") == fromJust (parseOO "f > + ^ + = cons ^ cons > z ^ f = g")
    ]
 where
  Just wqo =
    mergeAll
      [ "cons" =. "z"
      , "g" =. "nil"
      , "h" =. "s"
      , "cons" >. "g"
      , "cons" >. "h"
      , "h" >. "f"
      , "h" >. "g"
      ]

orderingTests :: (Hashable (oc Op), Show (oc Op), Ord (oc Op)) => (?impl :: WQOConstraints oc IO) => TestTree
orderingTests =
  testGroup
    "Ordering Tests"
    [ testCase "Simple1" $
        (@?= False) $
          rpoGTE "f(t1)" "g(t2)" `permits'` (t1Op =. t2Op)
    , testCase "Simple2" $
        (@?= True) $
          rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op =. t2Op))
    , testCase "Simple3" $
        (@?= True) $
          rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op >. t2Op))
    , testCase "Subterm" $
        (@?= True) $
          rpoGTE "f(g)" "f" == noConstraints ?impl
    , testCase "Intersect" $
        OC.isUnsatisfiable ?impl (OC.intersect ?impl (OC.singleton ?impl (f >. g)) (OC.singleton ?impl (g >. f)))
          >>= (@?= True)
    ]
 where
  permits' = permits ?impl