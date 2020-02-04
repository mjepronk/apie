module Apie.Internal.Env where

import Apie.ContentStore (ContentStore(..), HasContentStore(..))
import Apie.EventLog (EventLog(..), HasEventLog(..))
import Apie.Internal.Auth (Auth(..), HasAuth(..))

data Env = Env
    { envAuth :: Auth
    , envContentStore :: ContentStore
    , envEventLog :: EventLog
    }

instance HasAuth Env where
    getAuth = envAuth

instance HasContentStore Env where
    getContentStore = envContentStore

instance HasEventLog Env where
    getEventLog = envEventLog
