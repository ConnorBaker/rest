module Test.Aeson.Options (myOptions) where

import Data.Aeson (Options (..), defaultOptions)

myOptions :: Options
myOptions = defaultOptions{allNullaryToStringTag = False, omitNothingFields = True, tagSingleConstructors = True}