{-# LANGUAGE OverloadedStrings #-}

module Test.StrictOC where

import Data.Maybe (fromJust)
import Language.REST.Internal.OpOrdering (parseOO, (=.), (>.))
import Language.REST.Internal.WQO (mergeAll)
import Language.REST.WQOConstraints.Strict (
  noConstraints,
  permits,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "StrictOC Tests"
    [ testCase "Permits" $ ((@?= True) . permits noConstraints) wqo
    , testCase "Permits2" $ ((@?= True) . permits noConstraints . fromJust . parseOO) "+ = f = nil = s ∧ + > g ∧ + > h ∧ cons > + ∧ cons > g"
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
