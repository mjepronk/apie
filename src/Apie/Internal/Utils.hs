module Apie.Internal.Utils
    ( plainHeaders
    , jsonHeaders
    , notFound
    , badRequest
    , okResponse
    , errorResponse
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

notFound :: Response
notFound = responseLBS status404 plainHeaders "This path leads nowhere."

badRequest :: Response
badRequest = responseLBS status400 plainHeaders "Bad request"

okResponse :: ToJSON a => a -> Response
okResponse = responseLBS status200 jsonHeaders . encode

errorResponse :: Status -> Text -> Response
errorResponse s err =
    responseLBS s jsonHeaders . encode $ object
        [ "status" .= String "error"
        , "errorMessage" .= String err]

version :: Text
version = T.pack (showVersion Paths.version)
