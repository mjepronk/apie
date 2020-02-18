module Apie.EventLog
    ( EventLog(..)
    , HasEventLog(..)
    , httpPutEvent
    , httpGetEvent
    , httpGetEvents
    , defaultEventLog
    )
where

import RIO hiding (log)
import qualified RIO.Text as T
import RIO.Time (UTCTime, getCurrentTime)
import RIO.List (unzip, find, headMaybe, lastMaybe)
import RIO.Partial (fromJust)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?), (.!=), (.=),
    object, withObject, encode, decode, decodeStrict)
import Network.Wai (Request, Response, lazyRequestBody, queryString, responseLBS, requestHeaders)
import Network.HTTP.Types (HeaderName, status200, status304, status404, status412, status500, queryToQueryText)
import Network.HTTP.Types.Header (hIfMatch, hIfNoneMatch)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Apie.Internal.Auth (Auth(..), HasAuth(..))
import Apie.Internal.Log (Log(..), HasLog(..), LogError(..), appendLog, readLog, iterateLog)
import Apie.Internal.ISODateTime (ISODateTime(..))
import Apie.Internal.Store (Store(..), HasStore(..), Hash)
import Apie.Internal.User (User(..))
import Apie.Internal.Utils (okResponse, errorResponse, jsonHeaders)


data EventLog = EventLog
    { eventLogPath :: FilePath
    }

instance FromJSON EventLog where
    parseJSON = withObject "contentstore" $ \v -> EventLog
        <$> v .:? "path" .!= eventLogPath defaultEventLog

instance ToJSON EventLog where
    toJSON e = object [ "path" .= eventLogPath e ]

class HasEventLog env where
    getEventLog :: env -> EventLog

instance HasEventLog EventLog where
    getEventLog = id

instance HasStore EventLog where
    getStore x = Store (eventLogPath x)

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
    toJSON e =
        object
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
defaultEventLog = EventLog "events/default/"


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
        Nothing -> pure $ errorResponse status500 "Invalid JSON."
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
                Nothing -> pure (errorResponse status500 "Event JSON malformed.")
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
            Just events ->
                let events' = (\(h, e) -> e { hash=Just h}) <$> zip hs events
                in  case lastHash of
                        Just h -> pure (responseWithETag h events')
                        Nothing -> pure (okResponse events')
            Nothing ->
                pure (errorResponse status500 "Could not decode event.")
    else pure (errorResponse status304 "Already up to date.")
  where
    getQueryParam name q =
        case find ((== name) . fst) q of
            Just (_, v) -> T.unpack <$> v
            Nothing -> Nothing

responseWithETag :: ToJSON a => Hash -> a -> Response
responseWithETag hash =
    responseLBS status200 headers . encode
  where
    headers = jsonHeaders <> [("ETag", fromString hash)]

toString :: ByteString -> String
toString = T.unpack . decodeUtf8Lenient

getHeader :: Request -> HeaderName -> Maybe ByteString
getHeader req header = snd <$> headMaybe (filter (\(h, _) -> h == header) (requestHeaders req))
