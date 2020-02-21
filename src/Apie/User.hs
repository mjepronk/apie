module Apie.User
    ( httpUpdateUser
    )
where

import RIO
import qualified RIO.Text as T
import Apie.Internal.User (User(..), updateUser)
import Apie.Internal.Auth (HasAuth, getAuth, user)
import Apie.Internal.Utils (okResponse, errorResponse)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.=), decode, object, withObject)
import Network.HTTP.Types (status400)
import Network.Wai (Request, Response, lazyRequestBody)

data UpdateUser = UpdateUser
    { updatePassword :: !Text
    , updateInfo     :: !Text
    }

instance FromJSON UpdateUser where
    parseJSON = withObject "UpdateUser" $ \v -> UpdateUser
        <$> v .: "password"
        <*> v .: "info"

-- | Update the current user
httpUpdateUser :: HasAuth env => Request -> RIO env Response
httpUpdateUser req = do
    u <- asks (user . getAuth)
    body <- liftIO $ lazyRequestBody req
    case decode body of
        Just new -> do
            r <- updateUser (u { password=updatePassword new, info=updateInfo new  })
            case r of
                Right _ -> pure (okResponse statusOk)
                Left err -> pure (errorResponse status400 (T.pack (show err)))
        Nothing -> pure (errorResponse status400 "Invalid JSON.")
  where
    statusOk = object [ "status" .= String "ok" ]
