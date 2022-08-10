{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Language.REST.SMT (SolverHandle, killZ3, spawnZ3)
import qualified Language.REST.WQOConstraints.ADT as AC

import qualified Test.ExploredTerms
import qualified Test.KBO
import qualified Test.LPO
import qualified Test.LazyOC
import qualified Test.MultisetOrder
import qualified Test.PaperFigures
import qualified Test.OpOrdering
import qualified Test.Properties
import qualified Test.RPO
import qualified Test.SMT
import qualified Test.StrictOC
import qualified Test.WQO

import Test.Tasty (
    TestTree,
    defaultIngredients,
    defaultMainWithIngredients,
    testGroup,
 )
import Test.Tasty.Runners.JsonReporter (consoleAndJsonReporter)

main :: IO ()
main = do
    setLocaleEncoding utf8
    solver <- spawnZ3
    paperFiguresTests <- Test.PaperFigures.goldenTests
    defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients) $
        testGroup
            "Tests"
            [ Test.ExploredTerms.tests
            , Test.SMT.tests
            , Test.KBO.tests solver
            , Test.Properties.tests
            , Test.OpOrdering.tests
            , ocTests solver
            , Test.MultisetOrder.tests
            , Test.WQO.tests
            , implTests solver
            , paperFiguresTests
            ]
    killZ3 solver

ocTests :: SolverHandle -> TestTree
ocTests solver =
    testGroup
        "OC Tests"
        [ Test.LazyOC.tests
        , Test.StrictOC.tests
        , Test.RPO.tests
        , Test.OpOrdering.orderingTests
        ]
  where
    ?impl = AC.adtOC solver

implTests :: SolverHandle -> TestTree
implTests solver =
    testGroup
        "Implementation Tests"
        [ Test.KBO.implTests solver
        , Test.RPO.implTests solver
        , Test.LPO.implTests solver
        ]
  where
    ?impl = AC.adtOC solver