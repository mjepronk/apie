{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Apie.Diff.GToJSON
  ( genericToDiff )
where

import qualified RIO.Text as T
import Prelude
import GHC.Generics
import Data.Aeson (Object, Options, Object, Value, Object, (.=), fieldLabelModifier)
import Data.Aeson.Types
import Apie.Diff.Types (Diff(..))

genericToDiff :: (Generic a, ToJSONDiff (Rep a)) => Options -> a -> Value
genericToDiff opts = Object . toJSONDiff opts . from

class ToJSONDiff f where
    toJSONDiff :: Options -> f a -> Object

instance ToJSONDiff U1 where
    toJSONDiff _ U1 = mempty

instance ToJSONDiff f => ToJSONDiff (D1 x f) where
    toJSONDiff opts (M1 x) = toJSONDiff @f opts x

instance ToJSONDiff f => ToJSONDiff (C1 x f) where
    toJSONDiff opts (M1 x) = toJSONDiff @f opts x

instance (ToJSONDiff a, ToJSONDiff b) => ToJSONDiff (a :*: b) where
    toJSONDiff opts (a :*: b) = toJSONDiff opts a `mappend` toJSONDiff opts b

instance (Selector s, ToJSONDiff a) => ToJSONDiff (S1 s a) where
    toJSONDiff opts (M1 x) = label .= toJSONDiff opts x
      where
        label = T.pack $ fieldLabelModifier opts sname
        sname = selName (undefined :: M1 _i s _f _p)

instance {-# OVERLAPPING #-} (Selector s, ToJSON a) => ToJSONDiff (S1 s (K1 i a)) where
    toJSONDiff opts (M1 (K1 x)) = label .= toJSON x
      where
        label = T.pack $ fieldLabelModifier opts sname
        sname = selName (undefined :: M1 _i s _f _p)

instance {-# OVERLAPPING #-} (Selector s, ToJSON a) => ToJSONDiff (S1 s (K1 i (Diff a))) where
    toJSONDiff opts (M1 (K1 x)) =
        case x of
            Changed v -> label .= toJSON v
            Unchanged -> mempty
      where
        label = T.pack $ fieldLabelModifier opts sname
        sname = selName (undefined :: M1 _i s _f _p)
