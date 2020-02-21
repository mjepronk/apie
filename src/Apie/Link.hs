module Apie.Link
    ( httpPutLink
    )
where

import RIO
import RIO.Text as T
import RIO.FilePath (makeRelative, dropFileName)
import RIO.Directory (doesFileExist, canonicalizePath, pathIsSymbolicLink, removeFile)
import Apie.ContentStore (HasContentStore(..))
import Apie.Internal.Store (Hash, getFilePath)
import Apie.Internal.Utils (okResponse, errorResponse)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.=), decode, object, withObject)
import Network.HTTP.Types (status400)
import Network.Wai (Request, Response, lazyRequestBody)
import System.Posix.Files (createSymbolicLink)


data CreateLink = CreateLink
    { path :: FilePath
    , hash :: Hash
    }

instance FromJSON CreateLink where
    parseJSON = withObject "CreateLink" $ \v -> CreateLink
        <$> v .: "path"
        <*> v .: "hash"

httpPutLink :: HasContentStore env => Request -> RIO env Response
httpPutLink req = do
    body <- liftIO $ lazyRequestBody req
    case decode body of
        Just CreateLink { path, hash } -> do
            cs <- asks getContentStore
            fp <- runRIO cs (getFilePath hash)
            case fp of
                Just dest -> do
                    dest' <- liftIO $ createOrReplaceLink dest path
                    pure (okResponse (statusOk dest'))
                Nothing -> pure (errorResponse status400 "Hash does not exist in content store.")
        Nothing -> pure (errorResponse status400 "Invalid JSON.")
  where
    statusOk dest = object [ "status" .= String "ok", "destination" .= String (T.pack dest) ]

createOrReplaceLink :: FilePath -> FilePath -> IO FilePath
createOrReplaceLink dest ln = do
    dir <- canonicalizePath (dropFileName ln)
    dest' <- canonicalizePath dest
    let relDest = makeRelative dir dest'
    removeLinkIfExists ln
    createSymbolicLink relDest ln
    pure relDest
  where
    removeLinkIfExists ln' = do
        exists <- doesFileExist ln'
        when exists $ do
            symlink <- pathIsSymbolicLink ln'
            when symlink $ removeFile ln'
