{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Aeson.Orphans where

import Test.Aeson.Options (myOptions)
import Control.Exception (SomeException)
import Data.Aeson (ToJSON (toEncoding, toJSON), Value (Null))
import Data.Aeson.TH (deriveToJSON)
import Test.Tasty.Providers.ConsoleFormat (ResultDetailsPrinter)
import Test.Tasty.Runners (FailureReason (..), Outcome (..), Result (..))

instance ToJSON SomeException where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance ToJSON ResultDetailsPrinter where
  toJSON = const Null
  toEncoding = const (toEncoding Null)

$(deriveToJSON myOptions ''FailureReason)
$(deriveToJSON myOptions ''Outcome)
$(deriveToJSON myOptions ''Result)
