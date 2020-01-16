module Apie.EventLog
    ( EventLog(..)
    , httpPutEvent
    , httpGetEvent
    , httpGetEvents
    )
where

import RIO hiding (log)
import qualified RIO.Text as T
import RIO.Time (ZonedTime, getZonedTime)
import RIO.List (unzip, find)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?), (.=),
    object, withObject, encode, decode, decodeStrict)
import Network.Wai (Request, Response, responseLBS, lazyRequestBody, queryString)
import Network.HTTP.Types (status200, status404, status500, queryToQueryText)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Apie.Internal.Auth (Auth(..), HasAuth(..))
import Apie.Internal.Store (HasStore(..), Hash)
import Apie.Internal.Log (Log(..), HasLog(..), appendLog, readLog, iterateLog)
import Apie.Internal.Utils (jsonHeaders, errorResponse)


data EventLog = EventLog
    { auth :: Auth
    , log  :: Log
    }

instance HasAuth EventLog where
    getAuth = auth

instance HasLog EventLog where
    getLog = log

instance HasStore EventLog where
    getStore = store . getLog


data Event = Event
    { eventId   :: !UUID.UUID
    , eventType :: !String
    , body      :: !Value
    , createdBy :: !String
    , createdAt :: !ZonedTime
    , hash      :: Maybe Hash
    }

instance ToJSON Event where
    toJSON e =
        object
            [ "eventId"   .= eventId e
            , "eventType" .= eventType e
            , "body"      .= body e
            , "createdBy" .= createdBy e
            , "createdAt" .= createdAt e
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
    , newEventType :: !String
    , newBody      :: !Value
    }

instance FromJSON NewEvent where
    parseJSON = withObject "new event" $ \v -> NewEvent
        <$> v .:? "eventId"
        <*> v .:  "eventType"
        <*> v .:  "body"

-- TODO return error when UUID is invalid
-- TODO validate eventType
mkEvent :: HasAuth env => NewEvent -> RIO env Event
mkEvent (NewEvent { newEventId, newEventType, newBody }) = do
    eventId <- maybe (liftIO UUID.nextRandom) pure newEventId -- >>= UUID.fromText)
    createdBy <- asks (username . getAuth)
    createdAt <- liftIO getZonedTime
    pure $ Event
        { eventId
        , eventType=newEventType
        , body=newBody
        , createdBy
        , createdAt
        , hash=Nothing }

httpPutEvent :: (HasAuth env, HasLog env, HasStore env) => Request -> RIO env Response
httpPutEvent req = do
    body <- liftIO $ lazyRequestBody req
    case decode body of
        Just newEvent -> do
            event <- mkEvent newEvent
            hash <- appendLog Nothing (encode event)
            case hash of
                Right hash' -> pure $ okResponse (event { hash=Just (show hash') })
                Left err -> pure $ errorResponse status500 (T.pack . show $ err)
        Nothing -> pure $ errorResponse status500 "Invalid JSON."
  where
    okResponse event =
        responseLBS status200 jsonHeaders .
        encode $ object ["status" .= String "ok" , "event" .= event]

httpGetEvent :: HasStore env => Request -> Hash -> RIO env Response
httpGetEvent _req hash = do
    bs <- readLog hash
    case bs of
        Just bs' -> do
            case decodeStrict bs' of
                Just event -> do
                    pure (okResponse (event { hash=Just hash }))
                Nothing -> pure (errorResponse status500 "Event JSON malformed.")
        Nothing ->
            pure (errorResponse status404 "Event not found.")
  where
    okResponse event =
        responseLBS status200 jsonHeaders .
        encode $ object ["status" .= String "ok" , "event" .= event]

httpGetEvents :: (HasLog env, HasStore env) => Request -> RIO env Response
httpGetEvents req = do
    let q = queryToQueryText (queryString req)
        hashFrom = getQueryParam "from" q
        hashTo = getQueryParam "to" q
    (hs, bs) <- unzip <$> iterateLog hashFrom hashTo
    case traverse decodeStrict bs of
        Just events ->
            let events' = (\(h, e) -> e { hash=Just h}) <$> zip hs events
            in  pure (okResponse events')
        Nothing ->
            pure (errorResponse status500 "Could not decode event.")
  where
    okResponse events =
        responseLBS status200 jsonHeaders .
        encode $ object ["status" .= String "ok" , "events" .= events]

    getQueryParam name q =
        case find ((== name) . fst) q of
            Just (_, v) -> maybe Nothing (Just . T.unpack) v
            Nothing -> Nothing
