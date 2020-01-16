module Main where

{-

Main entrypoint for the CGI binary

-}
-- TODO: detect GATEWAY_INTERFACE environment variable if it's set perform CGI, if not use CLI mode

import RIO
import Network.Wai.Handler.CGI (run)

import Apie (app)

main :: IO ()
main = run app
