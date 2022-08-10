{-# LANGUAGE OverloadedStrings #-}

module Test.KBO where

import qualified Test.Arithmetic
import qualified Test.Completion
import Language.REST.KBO (kbo, kboGTE)
import Language.REST.OCAlgebra (OCAlgebra (isSat))
import Language.REST.RuntimeTerm (RuntimeTerm)
import Language.REST.SMT (SolverHandle)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: SolverHandle -> TestTree
tests solver =
  testGroup
    "KBO Tests"
    [ testCase "Commutativity" $ gte "f(a, b)" "f(b, a)" >>= (@?= True)
    , testCase "Associativity" $ gte "f(a, f(b, c))" "f((a b), c)" >>= (@?= True)
    , testCase "Distributivity" $ gte "f(a, g(b, c))" "g(f(a, b), f(a, c))" >>= (@?= False)
    , -- Co-pilot generated tests cases
      testCase "Idempotence" $ gte "f(a, a)" "f(a, a)" >>= (@?= True)
    , testCase "Transitivity" $ gte "f(a, b)" "f(b, c)" >>= \b -> gte "f(a, c)" "f(b, c)" >>= \c -> (@?= True) (b && c)
    ]
 where
  gte :: RuntimeTerm -> RuntimeTerm -> IO Bool
  gte t u = isSat (kbo solver) (kboGTE t u)

implTests :: SolverHandle -> TestTree
implTests solver =
  testGroup
    "KBO Implementation Tests"
    [ Test.Arithmetic.tests impl
    , Test.Completion.tests impl
    ]
 where
  impl = kbo solver