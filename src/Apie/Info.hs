module Apie.Info
    ( httpGetInfo
    )
where

import RIO
import qualified RIO.Text as T
import Data.Aeson (Value(..), (.=), object, encode)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS, requestHeaders)
import System.Environment (getExecutablePath, getEnvironment)

import Apie.ContentStore (HasContentStore(..))
import Apie.EventLog (HasEventLog(..))
import Apie.Internal.Auth (HasAuth(..))
import Apie.Internal.Utils (jsonHeaders, version)

httpGetInfo :: (HasAuth env, HasContentStore env, HasEventLog env) => Request -> RIO env Response
httpGetInfo req = do
    exec <- liftIO getExecutablePath
    env <- liftIO getEnvironment
    auth <- asks getAuth
    el <- asks getEventLog
    cs <- asks getContentStore
    let json = encode $ object
            [ "exec"    .= String (T.pack exec)
            , "version" .= String version
            , "auth"    .= auth
            , "env"     .= listToObject T.pack (String . T.pack) env
            , "headers" .= listToObject (T.pack . show) (String . decodeUtf8Lenient) (requestHeaders req)
            , "config"  .= object
              [ "eventlog" .= el
              , "contentstore" .= cs
              ]
            ]
    pure $ responseLBS status200 jsonHeaders json
  where
    listToObject :: (a -> Text) -> (b -> Value) -> [(a, b)] -> Value
    listToObject fa fb xs = object $ fmap (\(k, v) -> (fa k, fb v)) xs
