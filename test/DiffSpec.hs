{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
module DiffSpec (spec) where

import RIO
import Data.UUID (UUID)
import Test.Hspec
import Data.Aeson (ToJSON(..), FromJSON(..), Object, Value(..), defaultOptions,
  toJSON, genericParseJSON, genericToJSON)
import Data.Aeson.Types (parseMaybe)
import Data.HashMap.Strict (fromList)
import Apie.HKD (HKD)
import Apie.Diff (Diff(..), Diffable, apply, diff, genericParseDiff, genericToDiff)

data Domain
  = PersonCreated Person
  | PersonUpdated PersonDiff
  | PersonDeleted UUID

type Person = Person' Identity
type PersonDiff = Person' Diff
type PersonUnit = Person' (Const ())

data Person' f = Person
  { name      :: HKD f Text
  , age       :: HKD f (Maybe Int)
  , languages :: HKD f [Text]
  , available :: HKD f Bool
  } deriving (Generic, Diffable)

deriving instance Eq Person
deriving instance Eq PersonDiff
deriving instance Eq PersonUnit
deriving instance Show Person
deriving instance Show PersonDiff
deriving instance Show PersonUnit

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON PersonDiff where
  parseJSON = genericParseDiff defaultOptions

instance FromJSON PersonUnit where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions

instance ToJSON PersonDiff where
  toJSON = genericToDiff defaultOptions

instance ToJSON PersonUnit where
  toJSON = genericToJSON defaultOptions


spec :: Spec
spec = do
  describe "genericToDiff" $ do
    it "serializes a PersonDiff with one changed field" $
      toJSON setAge `shouldBe` Object (fromList [("age", Number 36)])
    it "serializes a PersonDiff with one nullified field" $
      toJSON removedAge `shouldBe` Object (fromList [("age", Null)])
    it "serializes a PersonDiff with a list field" $
      toJSON setLanguages `shouldBe` Object (fromList [("languages", Array ["Haskell", "Python"])])
    it "serializes a PersonDiff with empty list field" $
      toJSON removeLanguages `shouldBe` Object (fromList [("languages", Array [])])
    it "serializes a PersonDiff with a boolean field" $
      toJSON setAvailable `shouldBe` Object (fromList [("available", Bool False)])
  describe "genericParseDiff" $ do
    it "deserializes a PersonDiff with one changed field" $
      parseMaybe parseJSON (Object (fromList [("age", Number 36)])) `shouldBe` Just setAge
    it "deserializes a PersonDiff with one nullified field" $
      parseMaybe parseJSON (Object (fromList [("age", Null)])) `shouldBe` Just removedAge
    it "deserializes a PersonDiff with a list field" $
      parseMaybe parseJSON (Object (fromList [("languages", Array ["Haskell", "Python"])])) `shouldBe` Just setLanguages
    it "deserializes a PersonDiff with empty list field" $
      parseMaybe parseJSON (Object (fromList [("languages", Array [])])) `shouldBe` Just removeLanguages
    it "deserializes a PersonDiff with a boolean field" $
      parseMaybe parseJSON (Object (fromList [("available", Bool False)])) `shouldBe` Just setAvailable
  -- describe "HKD with Const ()" $ do
  --   it "deserializes a PersonUnit" $
  --     parseMaybe parseJSON (Object (fromList [("id", Number 1)])) `shouldBe`
  --       Just (Person 1 (Const ()) (Const ()) (Const ()) (Const ()) :: PersonUnit)
  describe "apply" $ do
    it "applies a single PersonDiff to a Person" $
      initialPerson `apply` setAge `shouldBe` initialPerson { age = Just 36 }
  describe "diff" $ do
    it "diff a single PersonDiff to a Person" $
      initialPerson `diff` initialPerson { age = Just 36 } `shouldBe` setAge



initialPerson :: Person
initialPerson = Person
  { name      = "Matthias"
  , age       = Nothing
  , languages = []
  , available = False
  }

setAge :: PersonDiff
setAge = Person
  { name      = Unchanged
  , age       = Changed (Just 36)
  , languages = Unchanged
  , available = Unchanged
  }

removedAge :: PersonDiff
removedAge = Person
  { name      = Unchanged
  , age       = Changed (Nothing)
  , languages = Unchanged
  , available = Unchanged
  }

setLanguages :: PersonDiff
setLanguages = Person
  { name      = Unchanged
  , age       = Unchanged
  , languages = Changed ["Haskell", "Python"]
  , available = Unchanged
  }

removeLanguages :: PersonDiff
removeLanguages = Person
  { name      = Unchanged
  , age       = Unchanged
  , languages = Changed []
  , available = Unchanged
  }

setAvailable :: PersonDiff
setAvailable = Person
  { name      = Unchanged
  , age       = Unchanged
  , languages = Unchanged
  , available = Changed False
  }
