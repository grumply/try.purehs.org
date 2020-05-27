module Home (page) where

import Pure.Data.Txt as Txt
import Pure.Elm hiding (Left,Right)
import Pure.Data.Lifted
import Pure.WebSocket

import Shared

import Control.Concurrent
import Data.Traversable
import System.IO

import Prelude hiding (or)
import Data.Int

foreign import javascript unsafe
  "$r = CodeMirror.fromTextArea($1,{mode: 'text/x-haskell', lineNumbers: true, theme: 'one-dark', tabSize: 2, extraKeys: { Tab: function(cm) { cm.replaceSelection('  ','end'); }}})" 
    start_editor_js 
      :: Node -> IO Editor

foreign import javascript unsafe
  "$r = CodeMirror.fromTextArea($1,{ lineNumbers: true, theme: 'one-dark', tabSize: 2 })" 
    start_viewer_js 
      :: Node -> IO Editor

foreign import javascript unsafe
  "$1.getValue()"
    get_value_js
      :: Editor -> IO Txt

foreign import javascript unsafe
  "$2.setValue($1)"
    set_value_js
      :: Txt -> Editor -> IO ()

newtype Editor = Editor JSV

data Model = Model 
  { editor :: Maybe Editor 
  , result :: Maybe (Either Txt String)
  }

data Msg 
  = StartEditor Node 
  | Compile 
  | Receive
  | SetValue (Maybe Txt)
  | SetResult (Either Txt String)

page :: WebSocket -> Maybe String -> View
page = curry (run (App [] [Receive] [] mdl update view))
  where
    mdl = Model Nothing Nothing

    update Compile (ws,_) mdl = do
      Just v <- for (editor mdl) get_value_js -- shouldn't fail
      remote backendAPI ws compile v (command . SetResult)
      pure mdl { result = Just (Left "Compiling") }
    update Receive (ws,mi) mdl = do
      for_ mi $ \i -> do
        remote backendAPI ws readModule i (command . SetValue)
      pure mdl
    update (StartEditor n) _ mdl = do
      e <- start_editor_js n
      pure mdl { editor = Just e }
    update (SetValue mt) _ mdl = do
      let t = maybe ("module Main where\n\nimport Pure\n\n-- requested module not found") id mt
      for_ (editor mdl) (set_value_js t)
      pure mdl
    update (SetResult r) _ mdl = do
      for_ r $ \h -> pushState ("/" <> toTxt h)
      pure mdl { result = Just r }

    view :: Elm Msg => (WebSocket,Maybe String) -> Model -> View
    view _ mdl =
      Div <| Themed @TryT |>
        [ Div <| Themed @EditorT |>
          [ Textarea <| WithHost (command . StartEditor) |> [ "module Main where\n\nimport Pure" ]
          ]
        , Div <| Themed @ResultT |>
          [ Button <| OnClick (\_ -> command Compile) |> [ "Compile" ]
          , maybe nothing (either failure success) (result mdl)
          ]
        ]
      where
        nothing   = Null
        failure m = viewer m 
        success r = Iframe <| Src ("/static/builds/" <> toTxt r <> "/Main.jsexe/index.html")

data ResultsModel = ResultsModel
  { results :: Maybe Editor }

data ResultsMsg 
  = ResultsReceive
  | StartResultsViewer Node

viewer = run (App [] [ResultsReceive] [] (ResultsModel Nothing) update view)
  where

    update ResultsReceive env mdl = do
      for_ (results mdl) (set_value_js env)
      pure mdl
    update (StartResultsViewer n) env mdl = do
      e <- start_viewer_js n
      set_value_js env e
      pure mdl { results = Just e }

    view env _ =
      Div <||>
        [ Textarea <| WithHost (command . StartResultsViewer)
        ]

data TryT
instance Theme TryT where
  theme c = void $
    is c .> do
      height =: (100%)
      width  =: (100%)

data EditorT
instance Theme EditorT where
  theme c = void $
    is c .> do
      display =: inline-block
      float =: left
      height =: (100%)
      width  =: (50%)

data ResultT
instance Theme ResultT where
  theme c = void $ do
    is c .> do
      background-color =: hex 0x282c34
      display =: inline-block
      float =: right
      height =: (100%)
      width  =: (50%)
    is c . child (tag Div) .> do
      height =: calc((100%) - 30px)
    is c . has (tag Button) .> do
      height =: 30px
    is c . has (tag Iframe) .> do
      background-color =: white
      padding-top =: 30px
      outline =: Pure.Elm.none
      width =: (100%)
      height =: (100%)
