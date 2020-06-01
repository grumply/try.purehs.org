{-# language DeriveAnyClass #-}
module Page.Themes where

import Page.Utils

import Pure.Elm

import Prelude hiding (or)

-- States
data EditingT   deriving Theme
data CompilingT deriving Theme
data SuccessT   deriving Theme
data FailureT   deriving Theme

data AppT
instance Theme AppT where
  theme c = void $ do
    is c .> do
      height =: (100%)

    is c . child (tag Div) .> do
      height =: (100%)

    is "*" . or is after . or is before .> do
      box-sizing  =: inherit
      webkit-box-sizing =: inherit

    is (tag Html) .> do
      box-sizing =: border-box

    is "body" .> do
      text-rendering =: "optimizeLegibility"
      webkit-font-smoothing =: antialiased
      moz-osx-font-smoothing =: grayscale

    -- page defaults

    is (tag Html) .> do
      height =: (100%)

    is "body" .> do
      margin =: 0
      height =: (100%)

    is "body" . child (tag Div) .> do
      width  =: (100%)
      height =: (100%)

    is (tag Html) . has ".CodeMirror" .> do
      height =: (100%)

data PageT
instance Theme PageT where
  theme c = pure ()

data CommandsT
instance Theme CommandsT where
  theme c = void $ 
    is c .> do
      width =: (100%)
      height =: 20px

data EditButtonT
instance Theme EditButtonT where
  theme c = void $ 
    is c $ do
      apply $ do
        display =: inline-block
        height  =: 20px

      splitWidth <%> do
        display =: none

data CompileButtonT
instance Theme CompileButtonT where
  theme c = void $ 
    is c .> do
      display =: inline-block
      height  =: 20px

data VimButtonT
instance Theme VimButtonT where
  theme c = void $ 
    is c $ do
      apply $ do
        display =: inline-block
        height  =: 20px

data EditorT
instance Theme EditorT where
  theme c = void $ do
    is c $ do
      apply $ do
        width =: (100%)
        height =: calc((100%) - 20px)

      splitWidth <%> do
        float =: left
        width  =: (50%)

    is (subtheme @EditingT) . has c $ do
      apply $ 
        display =: block

    is (subtheme @CompilingT) . has c $ do
      apply $
        display =: none

      splitWidth <%> do
        display =: inline-block

data ResultsT
instance Theme ResultsT where
  theme c = void $ do
    is c $ do
      apply $ do
        width =: (100%)
        height =: calc((100%) - 20px)

      splitWidth <%> do
        border-left =: solid <<>> 2px <<>> gray
        float =: right
        width  =: (50%)

    is (subtheme @EditingT) . has c $ do
      apply $
        display =: none

      splitWidth <%> do
        display =: inline-block

    is (subtheme @CompilingT) . has c $ do
      apply $
        display =: block

      splitWidth <%> do
        display =: inline-block

    has c . is (subtheme @SuccessT) $ do
      -- hide CodeMirror when compiling and compilation succeeds
      has ".CodeMirror" .> do
        display =: none

data FrameT
instance Theme FrameT where
  theme c = void $ do
    is c .> do
      display =: none
      background-color =: white
      outline =: none
      border =: none
      width =: (100%)
      height =: (100%)

    is (subtheme @SuccessT) . has c .> do
      display =: block