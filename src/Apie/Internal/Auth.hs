module Apie.Internal.Auth
    ( Auth(..)
    , HasAuth(..)
    , getAuthUser
    , authMiddleware
    , authSettings
    )
 where

import RIO
import RIO.List (headMaybe)
import Network.HTTP.Types (hAuthorization)
import Network.Wai (Middleware, Request, requestHeaders)
import Network.Wai.Middleware.HttpAuth (AuthSettings, basicAuth, extractBasicAuth)
import Data.Aeson (ToJSON(..), (.=), object)

import Apie.Internal.User (User(..), authenticate)

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

getAuthUser :: (MonadThrow m, MonadUnliftIO m) => Request -> m (Maybe (User ()))
getAuthUser req = do
    let hs = filter (\(h, _) -> h == hAuthorization) $ requestHeaders req
    case headMaybe hs of
        Just (_, bs) ->
            case extractBasicAuth bs of
                Just (u, p) -> do
                    x <- authenticate (decodeUtf8Lenient u) (decodeUtf8Lenient p)
                    case x of
                        Just user -> pure (Just user)
                        Nothing -> pure Nothing
                Nothing -> pure Nothing
        Nothing -> pure Nothing

authMiddleware :: Middleware
authMiddleware = basicAuth (\u p -> runRIO () $ do
    x <- authenticate (decodeUtf8Lenient u) (decodeUtf8Lenient p)
    pure (isJust x)) authSettings

authSettings :: AuthSettings
authSettings = "Apie"
