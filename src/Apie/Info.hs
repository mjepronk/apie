module Apie.Info
  ( httpGetInfo )
where

import RIO
import qualified RIO.Text as T
import Data.Aeson (Value(..), (.=), object, encode)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import System.Environment (getExecutablePath, getEnvironment)

import Apie.Internal.Utils (jsonHeaders, version)

httpGetInfo :: Request -> IO Response
httpGetInfo _req = do
    exec <- getExecutablePath
    env <- getEnvironment
    let json = encode $ object
            [ "exec"    .= String (T.pack exec)
            , "version" .= String version
            , "env"     .= listToObject env
            ]
    pure $ responseLBS status200 jsonHeaders json
  where
    listToObject :: [(String, String)] -> Value
    listToObject xs = object $ fmap (\(k, v) -> (T.pack k, String (T.pack v))) xs
