module Apie.ContentStore
    ( ContentStore(..)
    , HasContentStore(..)
    , URL
    , Hash
    , httpPutFile
    , httpGetFile
    , httpDeleteFile
    , defaultContentStore
    )
where

import RIO
import qualified RIO.Text as T
import RIO.Partial (fromJust)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), (.:?), (.!=), object, encode, withObject)
import Data.Aeson.Types (explicitParseFieldMaybe)
import RIO.FilePath (splitDirectories)
import Network.Wai (Request, Response, responseLBS, responseFile, lazyRequestBody)
import Network.HTTP.Types (status200, status404, status500)
import System.Posix.Types (FileMode)

import Apie.Internal.FileMode (toSymbolicRep, fromSymbolicRep, parseFileMode)
import Apie.Internal.Store (Store(..), HasStore(..), storeLBS, getFilePath,
    getRelFilePath, deleteFile)
import Apie.Internal.Utils (jsonHeaders, errorResponse)


type Hash = String
type URL = T.Text

data ContentStore = ContentStore
    { contentStorePath     :: FilePath
    , contentStoreURL      :: URL
    , contentStoreFileMode :: FileMode
    , contentStoreDirMode  :: FileMode
    }

instance FromJSON ContentStore where
    parseJSON = withObject "contentstore" $ \v -> ContentStore
        <$> v .:? "path" .!= contentStorePath defaultContentStore
        <*> v .:? "url" .!= contentStoreURL defaultContentStore
        <*> (explicitParseFieldMaybe parseFileMode v "fileMode" .!= contentStoreFileMode defaultContentStore)
        <*> (explicitParseFieldMaybe parseFileMode v "dirMode" .!= contentStoreDirMode defaultContentStore)

instance ToJSON ContentStore where
    toJSON c = object
        [ "path" .= contentStorePath c
        , "url" .= contentStoreURL c
        , "fileMode" .= toSymbolicRep (contentStoreFileMode c)
        , "dirMode" .= toSymbolicRep (contentStoreDirMode c)
        ]

instance HasStore ContentStore where
    getStore x = Store
        { path = contentStorePath x
        , fileMode = contentStoreFileMode x
        , dirMode = contentStoreDirMode x
        }

class HasContentStore env where
    getContentStore :: env -> ContentStore

instance HasContentStore ContentStore where
    getContentStore = id

data ContentStoreError = FileDoesNotExists

instance Show ContentStoreError where
    show FileDoesNotExists = "File does not exist in content store."

defaultContentStore :: ContentStore
defaultContentStore = ContentStore
    { contentStorePath = "media/"
    , contentStoreURL = "http://localhost:8000/media/"
    , contentStoreFileMode = fromJust (fromSymbolicRep "rw-r--r--")
    , contentStoreDirMode = fromJust (fromSymbolicRep "rwxr-xr-x")
    }

-- TODO return size and content type in okResponse
httpPutFile :: HasContentStore env => Request -> String -> RIO env Response
httpPutFile req filename = do
    body <- liftIO $ lazyRequestBody req
    cs <- asks getContentStore
    hash <- runRIO cs (storeLBS filename body)
    fp <- runRIO cs (getRelFilePath hash)
    case fp of
        Just fp' -> pure $ okResponse hash (filePathToURL cs fp')
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
httpGetFile :: HasContentStore env => Request -> Hash -> RIO env Response
httpGetFile _req hash = do
    cs <- asks getContentStore
    fp <- runRIO cs (getFilePath hash)
    case fp of
        Just fp' -> pure $ responseFile status200 [] fp' Nothing
        Nothing  -> pure $ errorResponse status404 "File does not exists."

httpDeleteFile :: HasContentStore env => Request -> Hash -> RIO env Response
httpDeleteFile _req hash = do
    cs <- asks getContentStore
    res <- runRIO cs (deleteFile hash)
    case res of
        Just _  -> pure $ okResponse
        Nothing -> pure $ errorResponse status500 "Could not delete file."
  where
    okResponse =
        responseLBS status200 jsonHeaders . encode $ object ["status" .= String "ok"]

filePathToURL :: ContentStore -> FilePath -> URL
filePathToURL ContentStore { contentStoreURL } fp =
    contentStoreURL <> T.intercalate "/" (T.pack <$> splitDirectories fp)
