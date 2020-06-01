module Page.Editor (editor) where

import Page.Themes
import Page.Utils

import Pure.Elm hiding (not)

data Model = Model Editor

data Msg = Startup | CreateEditor Node | Command Command | LoadResult LoadResult

editor :: View
editor = run (App [Startup] [] [] (Model def) update view) ()
  where
    update Startup _ mdl = do
      subscribeWith Command
      subscribeWith LoadResult
      pure mdl

    update (CreateEditor n) _ _ = do
      e <- start_editor_js n
      pure (Model e)

    update (Command c) _ (Model e) = do
      case c of
        Compile -> do
          v <- get_value_js e
          publish (Module v)
        Vim -> set_vim_js e
        _   -> pure ()
      pure (Model e)

    update (LoadResult r) _ (Model e) = do
      let m = case r of
                LoadSuccess s -> s
                LoadFailure   -> "module Main where\n\nimport Pure\n\n-- requested module not found\n\n"
      set_value_js m e
      pure (Model e)

    view _ mdl = 
      Div <| Themed @EditorT |>
        [ Textarea <| WithHost (command . CreateEditor) |> [ "module Main where\n\nimport Pure" ]
      ]

foreign import javascript unsafe
  "$r = CodeMirror['fromTextArea']($1,{ 'mode' : 'text/x-haskell', 'lineNumbers': true, 'tabSize': 2, 'extraKeys': { Tab: function(cm) { cm['replaceSelection']('  ','end'); }}})" 
    start_editor_js 
      :: Node -> IO Editor

