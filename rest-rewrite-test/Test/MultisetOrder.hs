module Test.MultisetOrder where

import Prelude hiding (GT)

import Control.Monad.Identity (Identity (..))
import Language.REST.Internal.MultiSet as M (MultiSet, fromList)
import Language.REST.Internal.MultisetOrder (multisetOrder)
import Language.REST.Types (Relation (GT, GTE))
import Language.REST.WQOConstraints as OC (
  ConstraintGen,
  intersectRelation,
 )
import Language.REST.WQOConstraints.Strict as SC (
  StrictOC,
  isUnsatisfiable,
  noConstraints,
  strictOC',
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

compareChar :: ConstraintGen impl Char Char Identity
compareChar impl r oc c1 c2 = Identity $ intersectRelation impl oc (c1, c2, r)

existingOC :: StrictOC Char
existingOC = OC.intersectRelation strictOC' SC.noConstraints ('a', 'c', GTE)

ms :: StrictOC Char -> MultiSet Char -> MultiSet Char -> Identity (StrictOC Char)
ms = multisetOrder compareChar strictOC' GTE

multisetNext :: Identity (StrictOC Char)
multisetNext = ms existingOC (M.fromList "bac") (M.fromList "aaaa")

unsat :: Identity (StrictOC Char)
unsat = do
  mn <- multisetNext
  return $ OC.intersectRelation strictOC' mn ('c', 'a', GT)

tests :: TestTree
tests =
  testGroup
    "MultisetOrder Tests"
    [ testCase "Constraints" $
        (@?= True) $
          SC.noConstraints /= runIdentity (ms SC.noConstraints (M.fromList "bc") (M.fromList "aa"))
    , testCase "Unsat" $
        (@?= True) $
          SC.isUnsatisfiable (runIdentity unsat)
    ]