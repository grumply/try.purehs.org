module Page.Results (results) where

import Page.Themes
import Page.Utils

import Pure.Data.Lifted
import Pure.Elm hiding (not)

import Control.Concurrent
import Data.Maybe

-- We use an editor to show error reports since it makes the system look consistent
data Model = Model { editor :: Editor, result :: Maybe String }

data Msg = Startup | Command Command | Compiled CompileResult | CreateEditor Node

results :: View
results = run (App [Startup] [] [] (Model def Nothing) update view) ()
  where
    update Startup _ mdl = subscribeWith Compiled *> subscribeWith Command *> pure mdl

    update (Command c) _ mdl =
      case c of
        Compile -> do
          set_value_js "Compiling..." (editor mdl)
          pure mdl { result = Nothing }
        Vim  -> set_vim_js (editor mdl) *> pure mdl
        Edit -> set_value_js "" (editor mdl) *> pure mdl { result = Nothing }

    update (Compiled cr) _ mdl =
      case cr of
        CompileFailure f -> do
          set_value_js f (editor mdl)
          pure mdl { result = Nothing }
        CompileSuccess s -> do
          pushState ("/" <> toTxt s)
          set_value_js "" (editor mdl)
          pure mdl { result = Just s }

    update (CreateEditor n) _ _ = do
      e <- start_viewer_js n
      pure (Model e Nothing)

    view _ (Model e r) =
      Div <| Themed @ResultsT . v |>
        [ Textarea <| WithHost (command . CreateEditor) |> (maybe [] ((:[]) . txt) r)
        , maybe Null (\s -> Iframe <| Themed @FrameT . Src (src s)) r
        ]
      where
        v | isJust r  = Themed @SuccessT
          | otherwise = Themed @FailureT
        src r = "http://try.purehs.org/static/builds/" <> toTxt r <> "/Main.jsexe/index.html"

foreign import javascript unsafe
  "$r = CodeMirror['fromTextArea']($1)" 
    start_viewer_js 
      :: Node -> IO Editor

