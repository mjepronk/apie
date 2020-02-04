-- A content-addressable store which uses SHA256 hashes as identifiers. This
-- functionality is used by both the Content Store and the Event Log
-- functionality of Apie.
module Apie.Internal.Store
    ( Store(..)
    , HasStore(..)
    , Hash
    , storeFile
    , storeLBS
    , withStoreFile
    , withStoreTempFile
    , getFilePath
    , getRelFilePath
    , deleteFile
    )
where

import Control.Monad (when)
import Crypto.Hash (Digest, HashAlgorithm, SHA256)
import Crypto.Hash.IO (hashMutableInit, hashMutableUpdate, hashMutableFinalize)
import RIO
import RIO.Directory (createDirectoryIfMissing, listDirectory,
    doesDirectoryExist, removeDirectory, removeFile, renameFile)
import RIO.File (withBinaryFile)
import RIO.FilePath ((</>), takeDirectory, makeRelative)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as LB
import qualified RIO.List as L

type Hash = String

data Store = Store
    { path :: FilePath
    }

class HasStore env where
    getStore :: env -> Store

instance HasStore Store where
    getStore = id

-- Important: this function moves the file from the path given to the store!
-- TODO ensure a proper filename
-- TODO ensure that existing file matches hash
-- TODO disallow cgi scripts
-- TODO ensure correct permissions (644 on files and 755 on dirs)
-- TODO renameFile is not (always) atomic, we should have a look at other implementations (RIO)
storeFile :: HasStore env => String -> FilePath -> RIO env Hash
storeFile filename fp = do
    store <- asks getStore
    digest <- liftIO (hashFile fp)
    let hash = show (digest :: Digest SHA256)
        dir = hashDir store hash
    createDirectoryIfMissing True dir
    contents <- listDirectory dir
    when (L.null contents) (renameFile fp (dir </> filename))
    pure hash

storeLBS :: HasStore env => String -> LB.ByteString -> RIO env Hash
storeLBS filename bs = do
    store <- asks getStore
    liftIO $ withStoreTempFile store filename $ \fp h -> do
        LB.hPut h bs
        hClose h
        runRIO store $ storeFile filename fp

getFilePath :: HasStore env => Hash -> RIO env (Maybe FilePath)
getFilePath hash = do
    store <- asks getStore
    let dir = hashDir store hash
    exists <- doesDirectoryExist dir
    if exists
    then do
        contents <- listDirectory dir
        case contents of
            (filename:_) -> pure (Just (dir </> filename))
            [] -> pure Nothing
    else pure Nothing

getRelFilePath :: HasStore env => Hash -> RIO env (Maybe FilePath)
getRelFilePath hash = do
    fp <- getFilePath hash
    store <- asks getStore
    pure $ makeRelative (path store) <$> fp

withStoreFile :: MonadUnliftIO m => Store -> Hash -> (Handle -> m a) -> m (Maybe a)
withStoreFile store hash action = do
    fp <- runRIO store $ getFilePath hash
    case fp of
        Just fp' -> Just <$> withBinaryFile fp' ReadMode action
        Nothing -> pure Nothing

withStoreTempFile :: MonadUnliftIO m => Store -> String -> (FilePath -> Handle -> m a) -> m a
withStoreTempFile store filename action = do
    let tmp = path store </> "tmp"
    createDirectoryIfMissing True tmp
    withTempFile tmp filename action

-- | Delete a file from the store by hash. It will automatically remove emty
-- directories left behind.
deleteFile :: HasStore env => Hash -> RIO env (Maybe ())
deleteFile hash = do
    fp <- getFilePath hash
    case fp of
        Just fp' -> do
            removeFile fp'
            removeDirIfEmpty (takeDirectory fp')
            removeDirIfEmpty (takeDirectory (takeDirectory fp'))
            pure (Just ())
        Nothing -> pure Nothing
  where
    removeDirIfEmpty fp = do
        contents <- listDirectory fp
        when (null contents) (removeDirectory fp)

-- | Get the directory path for a hash.
hashDir :: Store -> Hash -> FilePath
hashDir store hash = path store </> take 2 hash </> hash

-- | Hash a file at the given path.
hashFile :: HashAlgorithm ha => FilePath -> IO (Digest ha)
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
    context <- hashMutableInit
    let loop = do
            chunk <- B.hGetSome h 4096
            if B.null chunk
            then hashMutableFinalize context
            else do
                hashMutableUpdate context chunk
                loop
    loop
