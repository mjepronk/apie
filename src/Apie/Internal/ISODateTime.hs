module Apie.Internal.ISODateTime where

import RIO
import RIO.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON(..))

newtype ISODateTime = ISODateTime { fromISODateTime :: UTCTime }

instance ToJSON ISODateTime where
    toJSON = toJSON . formatTime defaultTimeLocale isoDateTimeFormat . fromISODateTime
        where isoDateTimeFormat = "%Y-%m-%dT%H:%M:%S%3QZ"
