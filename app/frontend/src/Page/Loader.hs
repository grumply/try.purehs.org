module Page.Loader (loader) where

import Page.Utils

import Shared

import Pure.Elm
import Pure.WebSocket

data Msg = Startup | Load Load

loader :: WebSocket -> View
loader = run (App [Startup] [] [] () update (\_ _ -> Null))
  where
    update :: Elm Msg => Msg -> WebSocket -> () -> IO ()
    update Startup _ _ = do
      subscribeWith Load
      pure ()
    update (Load (Hash h)) (ws,_) _ = do
      remote backendAPI ws readModule h (publish . maybe LoadFailure LoadSuccess)
      pure ()
