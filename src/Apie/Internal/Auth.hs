module Apie.Internal.Auth
    ( Auth(..)
    , HasAuth(..)
    , authenticateUser
    , authenticateResponse
    )
 where

import RIO
import RIO.List (find)
import Network.HTTP.Types (hAuthorization, status401)
import Network.Wai (Request, Response, requestHeaders)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Data.Aeson (ToJSON(..), (.=), object)

import Apie.Internal.User (User(..), authenticate)
import Apie.Internal.Utils (errorResponse')

newtype Auth = Auth { user :: User () }

instance ToJSON Auth where
    toJSON Auth { user } = object
        [ "user" .= object
            [ "email" .= email user
            , "info" .= info user
            ]
        ]

class HasAuth env where
    getAuth :: env -> Auth

instance HasAuth Auth where
    getAuth = id

authenticateUser :: (MonadThrow m, MonadUnliftIO m) => Request -> m (Maybe Auth)
authenticateUser req = do
    let authHeader = find (\(h, _) -> h == hAuthorization) (requestHeaders req)
    case authHeader of
        Just (_, bs) ->
            case extractBasicAuth bs of
                Just (u, p) -> do
                    x <- authenticate (decodeUtf8Lenient u) (decodeUtf8Lenient p)
                    case x of
                        Just user -> pure (Just (Auth { user=user }))
                        Nothing -> pure Nothing
                Nothing -> pure Nothing
        Nothing -> pure Nothing

authenticateResponse :: Response
authenticateResponse =
    errorResponse'
        [("WWW-Authenticate", "Basic realm=\"Apie\"")]
        status401
        "Basic authentication required"
