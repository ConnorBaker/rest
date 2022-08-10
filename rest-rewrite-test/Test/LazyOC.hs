module Test.LazyOC where

import Data.Maybe (fromJust)
import Language.REST.Internal.OpOrdering (OpOrdering, parseOO)
import Language.REST.WQOConstraints.Lazy as LC (
  addConstraint,
  isSatisfiable,
  noConstraints,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "LazyOC Tests"
    [ testCase "Intersect" $
        (@?=) False $
          LC.isSatisfiable $ LC.addConstraint (oo "g > f") (LC.addConstraint (oo "f > g ^ f > s") LC.noConstraints)
    ]
 where
  oo :: String -> OpOrdering
  oo = fromJust . parseOO