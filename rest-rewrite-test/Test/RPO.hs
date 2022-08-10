{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RPO where

import qualified Test.Arithmetic
import qualified Test.Completion
import Data.Hashable (Hashable)
import Data.Maybe as Mb (fromJust)
import Language.REST.Internal.OpOrdering as OpOrdering (
  empty,
  (>.),
 )
import Language.REST.Internal.WQO (mergeAll)
import Language.REST.OCToAbstract (lift)
import Language.REST.Op (Op (..))
import Language.REST.RPO (rpo, rpoGTE, synGTE)
import Language.REST.RuntimeTerm (RuntimeTerm (..))
import Language.REST.SMT (SolverHandle)
import Language.REST.WQOConstraints as OC (
  WQOConstraints (intersect, isSatisfiable, noConstraints),
  isUnsatisfiable,
  singleton,
 )
import qualified Language.REST.WQOConstraints.ADT as AC
import Test.Lib.Nat (s, z)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

bigLeft, bigRight :: RuntimeTerm
bigLeft = "f(h(s(g(z,nil))),f(g(z,z),nil,h(z)),f(z,nil,z) + g(z,s(z)))"
bigRight = "g(g(g(s(nil),z),s(z) + z),g(s(s(h(nil))),s(z)))"

massiveLeft :: RuntimeTerm
massiveLeft = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

massiveRight :: RuntimeTerm
massiveRight = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isnil(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By, cons(lqdcselectcons1(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), concat(lqdcselectcons2(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By))))"

listsLeft :: RuntimeTerm
listsLeft = "concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), concat(reverse(lqdcselectcons2(dsdOz)), cons(lqdcselectcons1(dsdOz), Nil)))"

listsRight :: RuntimeTerm
listsRight = "concat(concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), reverse(lqdcselectcons2(dsdOz))), cons(lqdcselectcons1(dsdOz), Nil))"

flattenLeft2 :: RuntimeTerm
flattenLeft2 = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

flattenRight2 :: RuntimeTerm
flattenRight2 = "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"

flattenSeq :: [RuntimeTerm]
flattenSeq =
  [ "concat(flatten(la1Bz), concat(flatten(ra1BA), nsa1By))"
  , "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"
  , "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"
  ]

rpoSeq ::
  (?impl :: WQOConstraints impl m, Show (impl Op), Eq (impl Op), Hashable (impl Op)) =>
  [RuntimeTerm] ->
  impl Op
rpoSeq = go (OC.noConstraints ?impl)
 where
  go c (t : u : _xss) = OC.intersect ?impl c (rpoGTE t u)
  go c _ = c

tests :: (Hashable (oc Op), Eq (oc Op), Show (oc Op)) => (?impl :: WQOConstraints oc IO) => TestTree
tests =
  testGroup
    "RPO Tests"
    [ testCase "RPO1" $
        (@?= True) $
          rpoGTE "f(z)" "g(s(z))" == OC.intersect ?impl (OC.singleton ?impl (f >. g)) (OC.singleton ?impl (f >. s))
    , testCase "RPO2" $
        isUnsatisfiable ?impl (OC.intersect ?impl (rpoGTE "f(z)" "g(s(z))") (rpoGTE "g(s(z))" "f(h(z))"))
          >>= (@?= True)
    , testCase "RPO3" $
        isSatisfiable ?impl (rpoGTE bigLeft bigRight)
          >>= (@?= True)
    , testCase "RPO4" $
        isUnsatisfiable ?impl (rpoGTE massiveLeft massiveRight)
          >>= (@?= True)
    , testCase "RPO5" $
        isSatisfiable ?impl (rpoGTE listsLeft listsRight)
          >>= (@?= True)
    , testCase "RPO6" $
        isSatisfiable ?impl (rpoGTE flattenLeft2 flattenRight2)
          >>= (@?= True)
    , testCase "RPOOrient" $
        isUnsatisfiable ?impl (rpoSeq flattenSeq)
          >>= (@?= True)
    , testCase "SynGTE" $
        (@?= True) $
          synGTE OpOrdering.empty (App s [App s [App g [App (Op "+") [App h [App s [App z []]], App z []], App s [App s [App g [App z [], App z []]]]]]]) (App z [])
    , testCase "SynGTE2" $
        (@?= True) $
          synGTE (Mb.fromJust $ mergeAll ["cons" >. g, f >. s, h >. g, h >. "nil"]) "s(cons(h(h(z)), f(nil, nil, z)))" "g(z, cons(g(nil, nil), s(s(z))))"
    ]
 where
  f = Op "f"
  g = Op "g"
  h = Op "h"

implTests :: SolverHandle -> TestTree
implTests solver =
  testGroup
    "RPO Implementation Tests"
    [ Test.Arithmetic.tests impl
    , Test.Completion.tests impl
    ]
 where
  impl = lift (AC.adtOC solver) rpo