{-# LANGUAGE OverloadedStrings #-}

module Test.LPO where

import qualified Test.Arithmetic
import qualified Test.Completion
import Language.REST.LPO (lpo)
import Language.REST.OCToAbstract (lift)
import Language.REST.SMT (SolverHandle)
import qualified Language.REST.WQOConstraints.ADT as AC
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.Patterns.Types as TP
import Test.Tasty.Runners (TestPattern (TestPattern))

testSkipper :: [String] -> Maybe TP.Expr
testSkipper [] = Nothing
testSkipper testNames = Just . foldr1 TP.And . map (TP.NE (TP.Field TP.NF) . TP.StringLit) $ testNames

implTests :: SolverHandle -> TestTree
implTests solver =
  testGroup
    "LPO Implementation Tests"
    [ -- Arith4, Arith4.1, Arith6 all fail with LPO
      localOption (TestPattern . testSkipper $ failingArithTests) $ Test.Arithmetic.tests impl
    , Test.Completion.tests impl
    ]
 where
  impl = lift (AC.adtOC solver) lpo
  failingArithTests = ["Arith4", "Arith4.1", "Arith6"]