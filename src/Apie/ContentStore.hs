module Apie.ContentStore
    ( ContentStore(..)
    , HasContentStore(..)
    , httpPutFile
    , httpGetFile
    , httpDeleteFile
    )
where

import RIO
import qualified RIO.Text as T
import Data.Aeson (Value(..), (.=), object, encode)
import RIO.FilePath (splitDirectories)
import Network.Wai (Request, Response, responseLBS, responseFile, lazyRequestBody)
import Network.HTTP.Types (status200, status404, status500)

import Apie.Internal.Store (Store(..), HasStore(..), storeLBS, getFilePath,
    getRelFilePath, deleteFile)
import Apie.Internal.Utils (jsonHeaders, errorResponse)


type Hash = String
type URL = T.Text

data ContentStore = ContentStore
    { store :: Store
    , url   :: URL
    }

instance HasStore ContentStore where
    getStore = store

class HasContentStore env where
    getContentStore :: env -> ContentStore

instance HasContentStore ContentStore where
    getContentStore = id

data ContentStoreError = FileDoesNotExists

instance Show ContentStoreError where
    show FileDoesNotExists = "File does not exist in content store."


-- TODO allow client to specify the filename
-- TODO return size and content type in okResponse
httpPutFile :: (HasContentStore env, HasStore env) => Request -> String -> RIO env Response
httpPutFile req filename = do
    body <- liftIO $ lazyRequestBody req
    hash <- storeLBS filename body
    fp <- getRelFilePath hash
    cstore <- asks getContentStore
    case fp of
        Just fp' -> pure $ okResponse hash (filePathToURL cstore fp')
        Nothing -> pure $ errorResponse status500 "Could not determine URL."
  where
    okResponse hash url =
        responseLBS status200 jsonHeaders .
        encode $ object
            [ "status" .= String "ok"
            , "hash" .= String (T.pack hash)
            , "url" .= String url
            ]

-- TODO would be nice if we could return a sensible content type
httpGetFile :: HasStore env => Request -> Hash -> RIO env Response
httpGetFile _req hash = do
    fp <- getFilePath hash
    case fp of
        Just fp' -> pure $ responseFile status200 [] fp' Nothing
        Nothing  -> pure $ errorResponse status404 "File does not exists."

httpDeleteFile :: HasStore env => Request -> Hash -> RIO env Response
httpDeleteFile _req hash = do
    res <- deleteFile hash
    case res of
        Just _  -> pure $ okResponse
        Nothing -> pure $ errorResponse status500 "Could not delete file."
  where
    okResponse =
        responseLBS status200 jsonHeaders . encode $ object ["status" .= String "ok"]

filePathToURL :: ContentStore -> FilePath -> URL
filePathToURL (ContentStore { url }) fp =
    url <> T.intercalate "/" (T.pack <$> splitDirectories fp)
