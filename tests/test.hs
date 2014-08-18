{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit

data Zero
data Succ a

type family Add a b
type instance Add Zero Zero = Zero
type instance Add Zero b = b
type instance Add (Succ a) b = Succ (Add a b)

type family Const a
type instance Const a = a

type One = Succ Zero
type Two = Succ One

data MyData1 = MyData1 { _md1Foo :: Text }
    deriving (Show, Eq, Generic)
instance ToJSON MyData1
instance FromJSON MyData1
data MyData2 rev = MyData2 {_md2Foo :: Text, _md2NewField :: Text}
    deriving (Show, Eq, Generic)

newtype VersionedJSON rev = VersionedJSON (J.Value) deriving (Show, Eq)
unVersionedJSON :: VersionedJSON rev -> J.Value
unVersionedJSON (VersionedJSON v) = v

type MyData2Rev = Two
instance ToJSON (MyData2 rev)
instance FromJSON (MyData2 MyData2Rev)
    where parseJSON o = let o' = unVersionedJSON
                                   (migrate
                                      (VersionedJSON o :: VersionedJSON Zero))
                        in J.genericParseJSON J.defaultOptions o'

class VersionedJSONMigration rev where
    type LastRev :: *
    migrate :: VersionedJSON rev -> VersionedJSON LastRev

instance VersionedJSONMigration Zero where
    type LastRev = MyData2Rev
    migrate (VersionedJSON (J.Object hm)) =
        let o = J.Object (H.insert "_md2NewField" "newFieldDefaultValue" hm)
        in migrate (VersionedJSON o :: VersionedJSON One)

instance VersionedJSONMigration One where
    type LastRev = MyData2Rev
    migrate (VersionedJSON v) =
        migrate (VersionedJSON v :: VersionedJSON Two)

instance VersionedJSONMigration Two where
    type LastRev = MyData2Rev
    migrate o = o

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Up-to-date deserialization" $
      (J.decode "{\"_md1Foo\": \"bar\"}" :: Maybe MyData1)
      @?=
      (Just (MyData1 {_md1Foo="bar"}))
  , testCase "New field with default was migrated" $
      (J.decode "{\"_md2Foo\": \"bar\"}" :: Maybe (MyData2 MyData2Rev))
      @?=
      Just (MyData2 "bar" "newFieldDefaultValue") ]
