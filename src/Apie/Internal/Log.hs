module Apie.Internal.Log
    ( Log(..)
    , LogError(..)
    , LogException(..)
    , HasLog(..)
    , appendLog
    , readLog
    , iterateLog
    )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Maybe (fromMaybe)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as LB
import qualified RIO.Text as T
import RIO hiding (log)
import RIO.Directory (doesFileExist)
import RIO.File (withBinaryFileDurableAtomic)
import RIO.FilePath ((</>))
import RIO.Partial (read)
import System.FileLock (FileLock, SharedExclusive(..), withFileLock)
import System.IO (hGetLine, hPutStrLn)
import Text.Printf (printf)

import Apie.Internal.Store (HasStore(..), Store(..), Hash, storeFile,
    withStoreFile, withStoreTempFile)

data Log = Log
    { store     :: !Store
    , extension :: !String
    }

class HasLog env where
    getLog :: env -> Log

instance HasLog Log where
    getLog = id

instance HasStore Log where
    getStore = store


type LogId = Int
type LogIndex = [(LogId, Hash)]

data LogError
    = DoesNotMatchExpected
    deriving (Show)

data LogException
    = LogIndexCorrupted

instance Show LogException where
    show LogIndexCorrupted = "Log index is corrupted, please contact administrator."

instance Exception LogException


-- TODO: should be a NOOP when eventId is already in the log
appendLog :: (HasLog env, HasStore env)
          => Maybe Hash -> LB.ByteString -> RIO env (Either LogError Hash)
appendLog expected bs = do
    log <- asks getLog
    withLogLock log Exclusive $ \_ -> do
        idx <- readIndex
        let revIdx = reverse idx
            (prevId, prevHash) = fromMaybe (0, show emptyHash) (headMay revIdx)
        if maybe True (== prevHash) expected
        then do
            let i = prevId + 1
                filename = printf "%010d" i <> extension log
            withStoreTempFile (store log) filename $ \fp h -> do
                -- We put the hash of the previous log entry so that we can
                -- easily verify the integrity of the whole log.
                liftIO $ hPutStrLn h prevHash
                liftIO $ LB.hPut h bs
                hClose h
                hash' <- storeFile filename fp
                let idx' = reverse ((i, hash') : revIdx)
                writeIndex idx'
                pure (Right hash')
        else pure (Left DoesNotMatchExpected)

readLog :: HasStore env => Hash -> RIO env (Maybe B.ByteString)
readLog hash' = do
    store <- asks getStore
    withStoreFile store hash' $ \h ->
        liftIO $ hGetLine h  -- Ignore the hash of the previous log entry.
            >> B.hGetContents h

iterateLog :: (HasLog env, HasStore env)
           => Maybe Hash -> Maybe Hash -> RIO env [(Hash, B.ByteString)]
iterateLog fromHash toHash = do
    hs <- inRange . fmap snd <$> readIndex
    bs <- sequence <$> traverse readLog hs
    case bs of
        Just bs' -> pure (zip hs bs')
        Nothing -> throwM LogIndexCorrupted
  where
    inRange :: [Hash] -> [Hash]
    inRange = takeUntil (\x -> maybe False (x ==) toHash) .
              dropWhile (\x -> maybe False (x /=) fromHash)

    takeUntil :: (a -> Bool) -> [a] -> [a]
    takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

withLogLock :: MonadUnliftIO m => Log -> SharedExclusive -> (FileLock -> m a) -> m a
withLogLock log mode action =
    withRunInIO $ \run ->
    withFileLock (lockFile log) mode $ \lock ->
    run (action lock)

writeIndex :: HasLog env => LogIndex -> RIO env ()
writeIndex idx = do
    log <- asks getLog
    withBinaryFileDurableAtomic (indexFile log) WriteMode $ \h ->
        traverse_ (B.hPutStr h . encodeUtf8 . encodeLine) idx
  where
    encodeLine (i, h) = T.pack (printf "%010d" i) <> " " <> T.pack h <> "\n"

readIndex :: HasLog env => RIO env LogIndex
readIndex = do
    log <- asks getLog
    let fp = indexFile log
    exists <- doesFileExist fp
    if exists
    then do
        xs <- readFileUtf8 fp
        pure (fmap decodeLine (T.lines xs))
    else
        pure []
  where
    decodeLine x =
        let (i, hash') = T.break (== ' ') x
        in  (read . T.unpack $ T.strip i, T.unpack $ T.strip hash')

indexFile :: Log -> FilePath
indexFile Log { store } = path store </> "index"

lockFile :: Log -> FilePath
lockFile Log { store } = path store </> "lock"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

emptyHash :: Digest SHA256
emptyHash = hash B.empty
