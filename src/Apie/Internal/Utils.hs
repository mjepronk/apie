module Apie.Internal.Utils
    ( plainHeaders
    , jsonHeaders
    , okResponse
    , notFound
    , badRequest
    , errorResponse
    , errorResponse'
    , version
    )
where

import RIO
import qualified RIO.Text as T
import Network.HTTP.Types (Status, ResponseHeaders, status200, status400, status404)
import Network.Wai (Response, responseLBS)
import Data.Aeson (ToJSON, Value(..), (.=), object, encode)
import Data.Version (showVersion)
import qualified Paths_apie as Paths


plainHeaders :: ResponseHeaders
plainHeaders = [("Content-Type", "text/plain")]

jsonHeaders :: ResponseHeaders
jsonHeaders = [("Content-Type", "application/json")]

okResponse :: ToJSON a => a -> Response
okResponse = responseLBS status200 jsonHeaders . encode

notFound :: Response
notFound = errorResponse status404 "This path leads nowhere."

badRequest :: Response
badRequest = errorResponse status400 "Bad request"

errorResponse :: Status -> Text -> Response
errorResponse = errorResponse' []

errorResponse' :: ResponseHeaders -> Status -> Text -> Response
errorResponse' hdrs s err =
    responseLBS s (jsonHeaders <> hdrs) . encode $ object
        [ "status" .= String "error"
        , "errorMessage" .= String err
        ]

version :: Text
version = T.pack (showVersion Paths.version)
