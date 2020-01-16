module Main where

{- Main entrypoint for the Warp HTTP server -}
-- TODO add wai-app-static for media

import RIO
import Prelude (putStrLn)
import qualified RIO.Text as T
import Network.Wai.Handler.Warp (run)

import Apie (app)

main :: IO ()
main = do
    let port = 8000
    putStrLn $ "Listening on port " <> show port <> "."
    run port app
