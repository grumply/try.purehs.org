module Page.Compiler (compiler) where

import Page.Utils

import Shared

import Pure.Elm
import Pure.WebSocket hiding (Build)

data Msg = Startup | Build Build

compiler :: WebSocket -> View
compiler = run (App [Startup] [] [] () update (\_ _ -> Null))
  where
    update :: Elm Msg => Msg -> WebSocket -> () -> IO ()
    update Startup _ _ = do
      subscribeWith Build
      pure ()
    update (Build (Module t)) ws _ = do
      remote backendAPI ws compile t (publish . either CompileFailure CompileSuccess)
      pure ()