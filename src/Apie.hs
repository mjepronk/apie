module Apie
    ( app
    ) where

import RIO hiding (log)
import RIO.Partial (fromJust)
import Network.Wai
import Network.HTTP.Types
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as LB

import Apie.ContentStore (httpPutFile, httpGetFile, httpDeleteFile)
import Apie.EventLog (httpPutEvent, httpGetEvent, httpGetEvents, httpGetEventsHead)
import Apie.Link (httpPutLink)
import Apie.Info (httpGetInfo)
import Apie.Internal.Auth (Auth(..), authMiddleware, getAuthUser)
import Apie.Internal.Config (parseConfig)
import Apie.Internal.Utils (badRequest, plainHeaders, notFound, version)

app :: Application
app = authMiddleware $ \req send -> do
    user <- fromJust <$> getAuthUser req
    let auth = Auth { user=user }
    env <- fromJust <$> parseConfig auth

    send =<< runRIO env (case pathInfo req of
      [] -> pure $ home req
      ["events"] ->
          case requestMethod req of
              "GET"  -> httpGetEvents req
              "PUT"  -> httpPutEvent req
              "POST" -> httpPutEvent req
              "HEAD" -> httpGetEventsHead req
              _      -> pure badRequest
      ["events", hash] ->
          case requestMethod req of
              "GET" -> httpGetEvent req (T.unpack hash)
              _     -> pure badRequest
      ["storage", hashOrFilename] ->
          case requestMethod req of
              "PUT"    -> httpPutFile req (T.unpack hashOrFilename)
              "POST"   -> httpPutFile req (T.unpack hashOrFilename)
              "GET"    -> httpGetFile req (T.unpack hashOrFilename)
              "DELETE" -> httpDeleteFile req (T.unpack hashOrFilename)
              _        -> pure badRequest
      ["links"] ->
          case requestMethod req of
              "PUT"  -> httpPutLink req
              "POST" -> httpPutLink req
              _      -> pure badRequest
      ["info"] ->
          case requestMethod req of
              "GET" -> httpGetInfo req
              _     -> pure badRequest
      _ -> pure notFound)

home :: Request -> Response
home _req = responseLBS status200 plainHeaders $
    LB.fromStrict . encodeUtf8 $ "Apie " <> version
