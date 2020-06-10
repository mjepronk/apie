module Apie.App
  ( mkApp
  )
where

import RIO
import RIO.Partial (fromJust)
import Network.Wai

import Apie.Internal.Auth (authenticateResponse, authenticateUser)
import Apie.Internal.Config (parseConfig)
import Apie.Internal.Env (Env)

mkApp :: (Request -> RIO Env Response) -> Application
mkApp router req send = do
    a <- authenticateUser req
    case a of
        Just auth -> do
            env <- fromJust <$> parseConfig auth
            send =<< runRIO env (router req)
        Nothing ->
            send authenticateResponse
