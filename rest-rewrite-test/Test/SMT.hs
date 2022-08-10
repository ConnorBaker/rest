module Test.SMT where

import qualified Data.Map as M
import Language.REST.SMT (parseModel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

model :: String
model =
  "(\n\
  \ (define-fun op_j () Int \n\
  \  1) \n\
  \(define-fun op_+ () Int \n\
  \  2) \n\
  \(define-fun op_i () Int \n\
  \  3) \n\
  \(define-fun op_s () Int \n\
  \  4) \n\
  \(define-fun op_< () Int \n\
  \  5) \n\
  \ )"

expected :: M.Map String String
expected =
  M.fromList
    [ ("op_j", "1")
    , ("op_+", "2")
    , ("op_i", "3")
    , ("op_s", "4")
    , ("op_<", "5")
    ]

tests :: TestTree
tests = testGroup "SMT Tests" [testCase "ParseModel" $ assertEqual "" expected (parseModel model)]
