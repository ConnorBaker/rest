module Test.WQO where

import Data.Maybe (isNothing)
import Language.REST.Internal.WQO as WQO (
  ExtendOrderingResult (ValidExtension),
  QORelation (QGT),
  WQO,
  empty,
  insert,
  insertMaybe,
  notStrongerThan,
  singleton,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "WQO Tests"
    [ testCase "NotStrongerThan" $ (@?= True) $ fg `notStrongerThan` fgyz
    , testCase "RejectInvalid" $ (@?= True) $ isNothing basicInvalid
    ]
 where
  ValidExtension fg = insert empty ("f", "g", QGT)
  ValidExtension fgyz = insert fg ("y", "z", QGT)

  basicInvalid :: Maybe (WQO Char)
  basicInvalid = do
    wqo1 <- WQO.singleton ('A', 'C', QGT)
    WQO.insertMaybe wqo1 ('C', 'A', QGT)
