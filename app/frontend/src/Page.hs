module Page (page) where

import Page.Commands
import Page.Editor
import Page.Compiler
import Page.Loader
import Page.Results
import Page.Themes
import Page.Utils

import Pure.Elm
import Pure.WebSocket

import Data.Foldable

data Model = Model { compiling :: Bool }

data Msg = Startup | Receive | Command Command

page :: WebSocket -> Maybe String -> View
page ws mh = run (App [Startup] [Receive] [] (Model False) update view) (ws,mh)
  where
    update Startup (_,mh) mdl = do
      subscribeWith Command
      pure mdl

    update Receive (_,mh) mdl = do
      for_ mh (publish . Hash)
      pure mdl

    update (Command c) _ mdl = do
      pure mdl 
        { compiling =  
          case c of
            Compile -> True
            _       -> False
        }

view (ws,mh) mdl = 
  let c | compiling mdl = Themed @CompilingT 
        | otherwise     = Themed @EditingT
  in Div <| Themed @PageT . c |>
      [ commands
      , editor
      , results
      , compiler ws
      , loader ws
      ]
