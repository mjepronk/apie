module Apie.Internal.Config
    ( parseConfig
    )
where

import RIO hiding (lookup)
import qualified RIO.ByteString as B
import Apie.Internal.Auth (Auth)
import Apie.Internal.Env (Env(..))
import Apie.ContentStore (defaultContentStore)
import Apie.EventLog (defaultEventLog)
import Data.Aeson (Value, (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser, parse)
import Data.Aeson.Parser (decodeStrictWith, value)

-- | Parse the Apie config file
parseConfig :: Auth -> IO (Maybe Env)
parseConfig auth = do
    contents <- B.readFile configFile
    case decodeStrictWith value (parse (pConfig auth)) contents of
        Just x -> pure (Just x)
        Nothing -> pure Nothing

-- Since we need `Auth` in order to be able to construct an `EventLog` object,
-- we use the lower level Parser interface (instead of `FromJSON` instances).
pConfig :: Auth -> Value -> Parser Env
pConfig auth = withObject "config" $ \v -> Env
    <$> pure auth
    <*> v .:? "contentstore" .!= defaultContentStore
    <*> v .:? "eventlog" .!= defaultEventLog

configFile :: FilePath
configFile = "apie.json"
