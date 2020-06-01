module Page.Utils where

import Pure

-- | splitWidth are screens that are wider than 992px
splitWidth :: Int
splitWidth = 992

-- | Construct a media query, given a minimum screen width.
(<%>) :: Int -> Styles Txt -> CSS Txt
(<%>) n = (.>) (atMedia ("screen and (min-width: " <> pxs n <> ")"))
infixr 1 <%>

data Build = Module Txt
data CompileResult = CompileSuccess String | CompileFailure Txt
  deriving Show

data Load = Hash String
data LoadResult = LoadSuccess Txt | LoadFailure
  deriving Show

data Command = Compile | Edit | Vim -- TODO: extend with editor options, like theme, keymap, etc....

foreign import javascript unsafe
  "$1['getValue']()"
    get_value_js
      :: Editor -> IO Txt

foreign import javascript unsafe
  "$2['setValue']($1)"
    set_value_js
      :: Txt -> Editor -> IO ()

foreign import javascript unsafe
  "$1['setOption']('keyMap','vim'); $1['setOption']('vimMode',true);"
    set_vim_js
      :: Editor -> IO ()

-- this didn't work for some reason
foreign import javascript unsafe
  "$1['setOption']('keyMap','sublime');"
    remove_vim_js
      :: Editor -> IO ()

newtype Editor = Editor JSV
  deriving Default
    via JSV
