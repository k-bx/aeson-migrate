{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import qualified Data.Aeson       as J
import qualified Data.Aeson.TH    as J
import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

data MyData1 = MyData1 { _md1Foo :: Text }
    deriving (Show, Eq)
$(J.deriveJSON J.defaultOptions ''MyData1)
data MyData2 = MyData2 {_md2Foo :: Text, _md2NewField :: Text}
    deriving (Show, Eq)
$(J.deriveJSON J.defaultOptions ''MyData2)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Up-to-date deserialization" $
      (J.decode "{\"_md1Foo\": \"bar\"}")
      @?=
      (Just (MyData1 {_md1Foo="bar"}))
  , testCase "New field with default was migrated" $
      J.decode "{\"foo\": \"bar\"}"
      @?=
      Just (MyData2 "bar" "newFieldDefaultValue") ]
