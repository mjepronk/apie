{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Apie.Diff.GParseJSON
  ( genericParseDiff )
where

import RIO hiding (to)
import qualified RIO.Text as T
import GHC.Generics
import Data.Aeson (Value, Object, Options, fieldLabelModifier, withObject)
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H

import Apie.Diff.Types (Diff(..))

genericParseDiff :: (Generic a, GParseJSON (Rep a)) => Options -> Value -> Parser a
genericParseDiff opts v = to <$> withObject "Object diff" (parseJSONDiff opts) v

class GParseJSON f where
  parseJSONDiff :: Options -> Object -> Parser (f a)

instance GParseJSON U1 where
  parseJSONDiff _ _ = pure U1

instance GParseJSON f => GParseJSON (D1 x f) where
  parseJSONDiff opts obj = M1 <$> parseJSONDiff @f opts obj

instance GParseJSON f => GParseJSON (C1 x f) where
  parseJSONDiff opts obj = M1 <$> parseJSONDiff @f opts obj

instance (GParseJSON a, GParseJSON b) => GParseJSON (a :*: b) where
  parseJSONDiff opts obj =
    (:*:) <$> parseJSONDiff opts obj
          <*> parseJSONDiff opts obj

instance (Selector s, FromJSON a) => GParseJSON (S1 s (K1 i a)) where
  parseJSONDiff opts obj = M1 . K1 <$> obj .: label
    where
      label = T.pack $ fieldLabelModifier opts sname
      sname = selName (undefined :: M1 _i s _f _p)

instance {-# OVERLAPPING #-} GParseJSON (S1 s (K1 i (Const () a))) where
  parseJSONDiff _ _ = M1 . K1 <$> pure (Const ())

instance {-# OVERLAPPING #-} (Selector s, FromJSON a) => GParseJSON (S1 s (K1 i (Diff a))) where
  parseJSONDiff opts obj =
      case H.lookup label obj of
        Just _ -> M1 . K1 . Changed <$> obj .: label
        Nothing -> M1 . K1 <$> pure Unchanged
    where
      label = T.pack $ fieldLabelModifier opts sname
      sname = selName (undefined :: M1 _i s _f _p)
