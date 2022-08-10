{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Runners.JsonReporter (
  jsonReporter,
  consoleAndJsonReporter,
) where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM

import Test.Aeson.Options (myOptions)
import Test.Aeson.Orphans ()
import Control.Monad (foldM)
import Data.Aeson (Options (fieldLabelModifier), fromEncoding, pairs, (.=))
import Data.Aeson.TH (deriveToJSON)
import Data.ByteString.Builder (hPutBuilder)
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text, pack, split, unpack)
import System.IO (IOMode (WriteMode), withBinaryFile)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Ingredients (Ingredient (TestReporter), composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option), OptionSet, lookupOption)
import Test.Tasty.Providers (IsTest)
import Test.Tasty.Runners (NumThreads (getNumThreads), Result, Status (..), StatusMap, TreeFold (foldGroup, foldSingle), foldTestTree, resultSuccessful, trivialFold)

newtype JSONPath = JSONPath FilePath
instance IsOption (Maybe JSONPath) where
  defaultValue = Nothing
  parseValue = Just . Just . JSONPath
  optionName = Tagged "json-path"
  optionHelp = Tagged "A file path to store the JSON-formatted test results"

newtype Meta = Meta (HashMap String String)
instance IsOption (Maybe Meta) where
  defaultValue = Just $ Meta HM.empty
  parseValue = Just . Just . Meta . helper
   where
    splitPair :: Text -> (String, String)
    splitPair t = case split (== '=') t of
      [a, b] -> (unpack a, unpack b)
      _ -> error "meta: invalid input; ensure that '=' appears exactly once, separating the from the value"

    helper :: String -> HashMap String String
    helper s = HM.fromList $ map splitPair $ split (== ',') (pack s)
  optionName = Tagged "meta"
  optionHelp = Tagged "A comma-delimited set of key-value pairs separated by an equals sign to add to the JSON output"

data JSONResult = JSONResult {_testPath :: [TestName], _result :: Result}
$(deriveToJSON myOptions{fieldLabelModifier = drop 1} ''JSONResult)

awaitResult :: StatusMap -> Int -> IO Result
awaitResult statusMap i = STM.atomically $
  case IM.lookup i statusMap of
    Nothing -> error "Looked up a test with an index that was out of bounds"
    Just tvarStatus ->
      STM.readTVar tvarStatus >>= \case
        Done result -> pure result
        _ -> STM.retry

-- Produces a list of list of tests names, where each list starts with the outermost test group and proceeds inwards, to the innermost test.
fullTestNames :: OptionSet -> TestTree -> [[TestName]]
fullTestNames = foldTestTree trivialFold{foldSingle = fs, foldGroup = fg}
 where
  fg :: OptionSet -> TestName -> [[TestName]] -> [[TestName]]
  fg _ groupName = map (groupName :)
  fs :: IsTest t => OptionSet -> TestName -> t -> [[TestName]]
  fs _ testName _ = [[testName]]

jsonReporter :: Ingredient
jsonReporter = TestReporter [Option $ Proxy @(Maybe JSONPath), Option $ Proxy @(Maybe Meta)] $ \options tree -> do
  JSONPath path <- lookupOption options
  Meta meta <- lookupOption options
  Just $ \statusMap -> do
    let numThreads :: Int
        numThreads = getNumThreads $ lookupOption options

        -- The test number in the status map corresponds to the list of test group names to locate that test.
        tests :: [(Int, [TestName])]
        tests = zip [0 ..] $ fullTestNames options tree

        go :: (Bool, [JSONResult]) -> (Int, [TestName]) -> IO (Bool, [JSONResult])
        go (accSuccess, accResults) (idx, fullTestName) = do
          result <- awaitResult statusMap idx
          let !success = resultSuccessful result && accSuccess
          let !results = JSONResult fullTestName result : accResults
          pure (success, results)
    (success, results) <- foldM go (True, []) tests

    return $ \elapsedTime ->
      success <$ do
        withBinaryFile path WriteMode $ \handle ->
          hPutBuilder handle $
            fromEncoding $
              pairs $
                "results" .= results
                  <> "time" .= elapsedTime
                  <> "success" .= success
                  <> "threads" .= numThreads
                  <> "testCount" .= length tests
                  <> if null meta then mempty else "meta" .= meta

consoleAndJsonReporter :: Ingredient
consoleAndJsonReporter = composeReporters consoleTestReporter jsonReporter
