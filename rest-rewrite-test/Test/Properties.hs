{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties where

import Data.Hashable (Hashable)
import Data.Maybe as Mb (fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import Language.REST.Internal.OpOrdering (OpOrdering)
import qualified Language.REST.Internal.PartialOrder as PO
import qualified Language.REST.Internal.WQO as WQO
import Language.REST.Op (Op (..))
import Language.REST.RPO (rpoGTE, synGTE)
import Language.REST.RuntimeTerm (RuntimeTerm (..))
import qualified Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.Lazy as LC
import qualified Language.REST.WQOConstraints.Strict as SC
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Property,
  choose,
  oneof,
  sized,
  vectorOf,
  (==>),
 )
import Test.QuickCheck.Monadic (assert, monadicIO, pre, run)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck as QC (
  QuickCheckMaxRatio (QuickCheckMaxRatio),
  QuickCheckMaxSize (QuickCheckMaxSize),
  QuickCheckTests (QuickCheckTests),
  testProperty,
 )
import Prelude hiding (EQ, GT)

type WQO = WQO.WQO

syms :: [(T.Text, Int)]
syms =
  [ ("f", 3)
  , ("g", 2)
  , ("h", 1)
  , ("s", 1)
  , ("z", 0)
  , ("+", 2)
  , ("cons", 2)
  , ("nil", 0)
  ]

gen_op :: Gen Op
gen_op =
  oneof (map (return . Op . fst) syms)

gen_po :: Gen (PO.PartialOrder Op)
gen_po =
  do
    num_ops <- choose (0, 10)
    go PO.empty num_ops
 where
  go :: PO.PartialOrder Op -> Int -> Gen (PO.PartialOrder Op)
  go po 0 = return po
  go po n = do
    f <- gen_op
    g <- gen_op
    let po' = fromMaybe po $ PO.insert po f g
    go po' (n - 1)

gen_wqo_steps :: Gen [(Op, Op, WQO.QORelation)]
gen_wqo_steps =
  do
    numOps <- choose (0, 10)
    vectorOf numOps go
  where
    go = do
      f <- gen_op
      g <- gen_op
      r <- arbitrary
      return (f, g, r)

toWQO :: [(Op, Op, WQO.QORelation)] -> WQO Op
toWQO = go WQO.empty where
  go wqo [] = wqo
  go wqo ((f, g, r):xs) =
    let
      wqo' = fromMaybe wqo $ WQO.insertMaybe wqo (f, g, r)
    in
      go wqo' xs

gen_wqo :: Gen (WQO Op)
gen_wqo = fmap toWQO gen_wqo_steps

gen_term :: Gen RuntimeTerm
gen_term = sized go
 where
  go :: Int -> Gen RuntimeTerm
  go sz = do
    (op, arity) <- oneof $ map return (filter ((<= sz) . snd) syms)
    args <- vectorOf arity (go (sz `div` (arity + 1)))
    return $ App (Op op) args

instance Arbitrary WQO.QORelation where
  arbitrary = oneof [return WQO.QGT, return WQO.QEQ]

instance Arbitrary Op where
  arbitrary = gen_op

instance Arbitrary RuntimeTerm where
  arbitrary = gen_term

instance Arbitrary (PO.PartialOrder Op) where
  arbitrary = gen_po

instance Arbitrary (WQO Op) where
  arbitrary = gen_wqo

prop_poTrans :: Op -> Op -> Op -> PO.PartialOrder Op -> Property
prop_poTrans f g h po =
  PO.gt po f g && PO.gt po g h ==> PO.gt po f h

prop_wqoTrans :: Op -> Op -> Op -> WQO.WQO Op -> Property
prop_wqoTrans f0 g0 h wqo = f0 `gte` g0 && g0 `gte` h ==> f0 `gte` h
 where
  gte f g = Mb.isJust $ WQO.getRelation wqo f g

prop_rpoTrans ::
  RuntimeTerm ->
  RuntimeTerm ->
  RuntimeTerm ->
  OpOrdering ->
  Property
prop_rpoTrans t u v wqo = synGTE wqo t u && synGTE wqo u v ==> synGTE wqo t v

prop_rpoCons ::
  (?impl :: OC.WQOConstraints impl m, Hashable (impl Op), Eq (impl Op), Show (impl Op)) =>
  OC.WQOConstraints impl IO ->
  RuntimeTerm ->
  RuntimeTerm ->
  Property
prop_rpoCons impl t u = monadicIO $ do
  isSat <- run $ OC.isSatisfiable impl constraints
  pre isSat
  assert $ synGTE ordering t u
 where
  constraints = rpoGTE t u
  ordering = Mb.fromJust (OC.getOrdering impl constraints)

prop_permits :: [(Op, Op, WQO.QORelation)] -> Bool
prop_permits steps = SC.permits SC.noConstraints (toWQO steps)

tests :: TestTree
tests =
  localOption (QuickCheckTests 100) $
    localOption (QuickCheckMaxSize 100) $
      testGroup
        "QuickCheck Tests"
        [ localOption (QuickCheckMaxRatio 3) $ testProperty "PROP_PERMITS" prop_permits
        , localOption (QuickCheckMaxRatio 200) $ testProperty "Transitivity of PO" prop_poTrans
        , localOption (QuickCheckMaxRatio 10) $ testProperty "Transitivity of WQO" prop_wqoTrans
        , localOption (QuickCheckMaxRatio 30) $ testProperty "Transitivity of RPO" prop_rpoTrans
        , localOption (QuickCheckMaxRatio 1) $ localOption (QuickCheckTests 5) $ testProperty "RPO aligns with concrete" (prop_rpoCons ?impl)
        ]
 where
  ?impl = LC.lazyOC
