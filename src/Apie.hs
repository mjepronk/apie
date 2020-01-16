module Apie
    ( app
    ) where

import RIO hiding (log)
import Network.Wai
import Network.HTTP.Types
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as LB
import RIO.Directory (canonicalizePath)

import Apie.ContentStore (ContentStore(..), httpPutFile, httpGetFile, httpDeleteFile)
import Apie.EventLog (EventLog(..), httpPutEvent, httpGetEvent, httpGetEvents)
import Apie.Info
import Apie.Internal.Auth (Auth(..))
import Apie.Internal.Store (Store(..))
import Apie.Internal.Log (Log(..))
import Apie.Internal.Utils

app :: Application
app req send = do
    -- Media store
    mediaPath <- canonicalizePath "media"
    let store = Store mediaPath
    let cstore = ContentStore store "http://localhost:8000/media/"

    -- Event log
    eventLogPath <- canonicalizePath "events/default/"
    let auth = Auth { username="mjepronk" }
    let logStore = Store { root=eventLogPath }
    let log = Log logStore ".json" []
    let eventLog = EventLog auth log

    send =<< case pathInfo req of
        [] -> pure $ home req
        ["events"] ->
            case requestMethod req of
                "GET"  -> runRIO eventLog $ httpGetEvents req
                "PUT"  -> runRIO eventLog $ httpPutEvent req
                "POST" -> runRIO eventLog $ httpPutEvent req
                _      -> pure badRequest
        ["events", hash] ->
            case requestMethod req of
                "GET" -> runRIO eventLog $ httpGetEvent req (T.unpack hash)
                _     -> pure badRequest
        ["storage", hashOrFilename] ->
            case requestMethod req of
                "PUT"    -> runRIO cstore $ httpPutFile req (T.unpack hashOrFilename)
                "POST"   -> runRIO cstore $ httpPutFile req (T.unpack hashOrFilename)
                "GET"    -> runRIO cstore $ httpGetFile req (T.unpack hashOrFilename)
                "DELETE" -> runRIO cstore $ httpDeleteFile req (T.unpack hashOrFilename)
                _        -> pure badRequest
        ["info"] ->
            case requestMethod req of
                "GET" -> httpGetInfo req
                _     -> pure badRequest
        _ -> pure notFound

home :: Request -> Response
home _req = responseLBS status200 plainHeaders $
    LB.fromStrict . encodeUtf8 $ "Apie " <> version
