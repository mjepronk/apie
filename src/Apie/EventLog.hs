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
import RIO.List (unzip, find)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?), (.!=), (.=),
    object, withObject, encode, decode, decodeStrict)
import Network.Wai (Request, Response, lazyRequestBody, queryString)
import Network.HTTP.Types (status404, status500, queryToQueryText)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Apie.Internal.Auth (Auth(..), HasAuth(..))
import Apie.Internal.Log (Log(..), HasLog(..), appendLog, readLog, iterateLog)
import Apie.Internal.ISODateTime (ISODateTime(..))
import Apie.Internal.Store (Store(..), HasStore(..), Hash)
import Apie.Internal.User (User(..))
import Apie.Internal.Utils (okResponse, errorResponse)


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
mkEvent (NewEvent { newEventId, newEventType, newBody }) = do
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

-- TODO: should be a NOOP when eventId is already in the log
httpPutEvent :: (HasAuth env, HasEventLog env) => Request -> RIO env Response
httpPutEvent req = do
    body <- liftIO $ lazyRequestBody req
    case decode body of
        Just newEvent -> do
            event <- mkEvent newEvent
            el <- asks getEventLog
            hash <- runRIO el (appendLog Nothing (encode event))
            case hash of
                Right hash' -> pure $ okResponse (event { hash=Just (show hash') })
                Left err -> pure $ errorResponse status500 (T.pack . show $ err)
        Nothing -> pure $ errorResponse status500 "Invalid JSON."

httpGetEvent :: HasEventLog env => Request -> Hash -> RIO env Response
httpGetEvent _req hash = do
    el <- asks getEventLog
    bs <- runRIO el (readLog hash)
    case bs of
        Just bs' -> do
            case decodeStrict bs' of
                Just event -> do
                    pure (okResponse (event { hash=Just hash }))
                Nothing -> pure (errorResponse status500 "Event JSON malformed.")
        Nothing ->
            pure (errorResponse status404 "Event not found.")

httpGetEvents :: HasEventLog env => Request -> RIO env Response
httpGetEvents req = do
    let q = queryToQueryText (queryString req)
        hashFrom = getQueryParam "from" q
        hashTo = getQueryParam "to" q
    el <- asks getEventLog
    (hs, bs) <- unzip <$> runRIO el (iterateLog hashFrom hashTo)
    case traverse decodeStrict bs of
        Just events ->
            let events' = (\(h, e) -> e { hash=Just h}) <$> zip hs events
            in  pure (okResponse events')
        Nothing ->
            pure (errorResponse status500 "Could not decode event.")
  where
    getQueryParam name q =
        case find ((== name) . fst) q of
            Just (_, v) -> T.unpack <$> v
            Nothing -> Nothing
