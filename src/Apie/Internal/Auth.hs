module Apie.Internal.Auth where

import RIO

data Auth = Auth { username :: !String }

class HasAuth env where
    getAuth :: env -> Auth

instance HasAuth Auth where
    getAuth = id
