module Apie
    ( User(..)
    , Auth(..)
    , Env(..)
    , parseConfig
    , mkApp
    , app
    )
where

import RIO
import qualified RIO.ByteString.Lazy as LB
import qualified RIO.Text as T
import Network.HTTP.Types
import Network.Wai

import Apie.App (mkApp)
import Apie.ContentStore (httpDeleteFile, httpGetFile, httpPutFile, httpVerifyStore)
import Apie.EventLog (httpGetEvent, httpGetEvents, httpGetEventsHead, httpPutEvent, httpVerifyLog)
import Apie.Info (httpGetInfo)
import Apie.Internal.Auth (Auth(..))
import Apie.Internal.Config (parseConfig)
import Apie.Internal.Env (Env(..))
import Apie.Internal.User (User(..))
import Apie.Internal.Utils (badRequest, notFound, plainHeaders, version)
import Apie.User (httpUpdateUser)

app :: Application
app = mkApp router

router :: Request -> RIO Env Response
router req =
    case pathInfo req of
        [] -> pure $ home req
        ["events"] ->
            case requestMethod req of
                "GET"  -> httpGetEvents req
                "PUT"  -> httpPutEvent req
                "POST" -> httpPutEvent req
                "HEAD" -> httpGetEventsHead req
                _      -> pure badRequest
        ["events", "verify"] ->
            case requestMethod req of
                "POST" -> httpVerifyLog req
                _      -> pure badRequest
        ["events", hash] ->
            case requestMethod req of
                "GET" -> httpGetEvent req (T.unpack hash)
                _     -> pure badRequest
        ["storage", "verify"] ->
            case requestMethod req of
                "POST" -> httpVerifyStore req
                _      -> pure badRequest
        ["storage", hashOrFilename] ->
            case requestMethod req of
                "PUT"    -> httpPutFile req (T.unpack hashOrFilename)
                "POST"   -> httpPutFile req (T.unpack hashOrFilename)
                "GET"    -> httpGetFile req (T.unpack hashOrFilename)
                "DELETE" -> httpDeleteFile req (T.unpack hashOrFilename)
                _        -> pure badRequest
        ["info"] ->
            case requestMethod req of
                "GET" -> httpGetInfo req
                _     -> pure badRequest
        ["user", "update"] ->
            case requestMethod req of
                "POST" -> httpUpdateUser req
                _      -> pure badRequest
        _ -> pure notFound

home :: Request -> Response
home _req = responseLBS status200 plainHeaders $
    LB.fromStrict . encodeUtf8 $ "Apie " <> version
