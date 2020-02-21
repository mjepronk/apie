module Apie.EventLog
    ( EventLog(..)
    , HasEventLog(..)
    , httpPutEvent
    , httpGetEvent
    , httpGetEvents
    , httpGetEventsHead
    , defaultEventLog
    )
where

import RIO hiding (log)
import qualified RIO.Text as T
import RIO.Time (UTCTime, getCurrentTime)
import RIO.List (unzip, find, lastMaybe)
import RIO.Partial (fromJust)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?), (.!=), (.=),
    object, withObject, encode, decode, decodeStrict)
import Data.Aeson.Types (explicitParseFieldMaybe)
import Network.Wai (Request, Response, lazyRequestBody, queryString, responseLBS, requestHeaders)
import Network.HTTP.Types (HeaderName, status200, status304, status400, status404, status412, queryToQueryText)
import Network.HTTP.Types.Header (hIfMatch, hIfNoneMatch)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Posix.Types (FileMode)

import Apie.Internal.Auth (Auth(..), HasAuth(..))
import Apie.Internal.FileMode (toSymbolicRep, fromSymbolicRep, parseFileMode)
import Apie.Internal.ISODateTime (ISODateTime(..))
import Apie.Internal.Log (Log(..), HasLog(..), LogError(..), appendLog, readLog, iterateLog)
import Apie.Internal.Store (Store(..), HasStore(..), Hash)
import Apie.Internal.User (User(..))
import Apie.Internal.Utils (okResponse, errorResponse, jsonHeaders)


data EventLog = EventLog
    { eventLogPath :: FilePath
    , eventLogFileMode :: FileMode
    , eventLogDirMode  :: FileMode
    }

instance FromJSON EventLog where
    parseJSON = withObject "contentstore" $ \v -> EventLog
        <$> v .:? "path" .!= eventLogPath defaultEventLog
        <*> (explicitParseFieldMaybe parseFileMode v "fileMode" .!= eventLogFileMode defaultEventLog)
        <*> (explicitParseFieldMaybe parseFileMode v "dirMode" .!= eventLogDirMode defaultEventLog)

instance ToJSON EventLog where
    toJSON e = object
        [ "path" .= eventLogPath e
        , "fileMode" .= toSymbolicRep (eventLogFileMode e)
        , "dirMode" .= toSymbolicRep (eventLogDirMode e)
        ]

class HasEventLog env where
    getEventLog :: env -> EventLog

instance HasEventLog EventLog where
    getEventLog = id

instance HasStore EventLog where
    getStore x = Store
        { path = eventLogPath x
        , fileMode = eventLogFileMode x
        , dirMode = eventLogDirMode x
        }

instance HasLog EventLog where
    getLog x = Log (getStore x) ".json"


data Event = Event
    { eventId   :: !UUID.UUID
    , eventType :: !T.Text
    , body      :: !Value
    , createdBy :: !T.Text
    , createdAt :: !UTCTime
    , hash      :: Maybe Hash
    }

instance ToJSON Event where
    toJSON e = object
        [ "eventId"   .= eventId e
        , "eventType" .= eventType e
        , "body"      .= body e
        , "createdBy" .= createdBy e
        , "createdAt" .= ISODateTime (createdAt e)
        , "hash"      .= hash e
        ]

instance FromJSON Event where
    parseJSON = withObject "stored event" $ \v -> Event
        <$> v .:  "eventId"
        <*> v .:  "eventType"
        <*> v .:  "body"
        <*> v .:  "createdBy"
        <*> v .:  "createdAt"
        <*> v .:? "hash"

data NewEvent = NewEvent
    { newEventId   :: Maybe UUID.UUID
    , newEventType :: !T.Text
    , newBody      :: !Value
    }

instance FromJSON NewEvent where
    parseJSON = withObject "new event" $ \v -> NewEvent
        <$> v .:? "eventId"
        <*> v .:  "eventType"
        <*> v .:  "body"

defaultEventLog :: EventLog
defaultEventLog = EventLog
    { eventLogPath = "events/default/"
    , eventLogFileMode = fromJust (fromSymbolicRep "r--r-----")
    , eventLogDirMode = fromJust (fromSymbolicRep "rwxr-x---")
    }


-- TODO return error when UUID is invalid
-- TODO validate eventType
mkEvent :: HasAuth env => NewEvent -> RIO env Event
mkEvent NewEvent { newEventId, newEventType, newBody } = do
    eventId <- maybe (liftIO UUID.nextRandom) pure newEventId -- >>= UUID.fromText)
    createdBy <- asks (email . user . getAuth)
    createdAt <- liftIO getCurrentTime
    pure $ Event
        { eventId
        , eventType=newEventType
        , body=newBody
        , createdBy
        , createdAt
        , hash=Nothing }

httpPutEvent :: (HasAuth env, HasEventLog env) => Request -> RIO env Response
httpPutEvent req = do
    body <- liftIO $ lazyRequestBody req
    let expected = toString <$> getHeader req hIfMatch
    case decode body of
        Just newEvent -> do
            event <- mkEvent newEvent
            el <- asks getEventLog
            hash <- runRIO el (appendLog expected (encode event))
            case hash of
                Right hash' -> pure $ okResponse (event { hash=Just (show hash') })
                Left DoesNotMatchExpected -> pure $ errorResponse status412 (expectedErr expected)
        Nothing -> pure $ errorResponse status400 "Invalid JSON."
  where
    expectedErr :: Maybe Hash -> Text
    expectedErr = ("Event log does not match expected version " <>) . fromString . fromJust

httpGetEvent :: HasEventLog env => Request -> Hash -> RIO env Response
httpGetEvent _req hash = do
    el <- asks getEventLog
    bs <- runRIO el (readLog hash)
    case bs of
        Just bs' ->
            case decodeStrict bs' of
                Just event -> pure (responseWithETag hash (event { hash=Just hash }))
                Nothing -> pure (errorResponse status400 "Event JSON malformed.")
        Nothing ->
            pure (errorResponse status404 "Event not found.")

httpGetEvents :: HasEventLog env => Request -> RIO env Response
httpGetEvents req = do
    -- TODO: add `limit` query parameter
    let q = queryToQueryText (queryString req)
        hashFrom = getQueryParam "from" q
        hashTo = getQueryParam "to" q
    el <- asks getEventLog
    (hs, bs) <- unzip <$> runRIO el (iterateLog hashFrom hashTo)
    let lastHash = lastMaybe hs
    if lastHash /= (toString <$> getHeader req hIfNoneMatch)
    then
        case traverse decodeStrict bs of
            Just events -> do
                let events' = (\(h, e) -> e { hash=Just h}) <$> zip hs events
                case lastHash of
                    Just h -> pure (responseWithETag h events')
                    Nothing -> pure (okResponse events')
            Nothing ->
                pure (errorResponse status400 "Could not decode event.")
    else pure (errorResponse status304 "Already up to date.")
  where
    getQueryParam name q =
        case find ((== name) . fst) q of
            Just (_, v) -> T.unpack <$> v
            Nothing -> Nothing

httpGetEventsHead :: HasEventLog env => Request -> RIO env Response
httpGetEventsHead _ = do
    el <- asks getEventLog
    (hs, _) <- unzip <$> runRIO el (iterateLog Nothing Nothing)
    pure (responseLBS status200 (headers (lastMaybe hs)) mempty)
  where
    headers (Just h) = jsonHeaders <> [("ETag", fromString h), ("X-Apie-Hash", fromString h)]
    headers Nothing  = jsonHeaders

responseWithETag :: ToJSON a => Hash -> a -> Response
responseWithETag h =
    responseLBS status200 headers . encode
  where
    headers = jsonHeaders <> [("ETag", fromString h), ("X-Apie-Hash", fromString h)]

toString :: ByteString -> String
toString = T.unpack . decodeUtf8Lenient

getHeader :: Request -> HeaderName -> Maybe ByteString
getHeader req header = snd <$> find ((== header) . fst) (requestHeaders req)
