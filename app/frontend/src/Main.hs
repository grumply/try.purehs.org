{-# language QuasiQuotes #-}
module Main where

import Pure.Elm.Application
import Pure.WebSocket

import System.IO

import Prelude hiding (or)
import Data.Int

import qualified Home

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- clientWS "204.48.20.19" 8080
  inject body (Div <| Themed @AppT |> [ app ws ])

data Route = HomeR | RefR String

instance Routes Route where
  home = HomeR

  routes = do
    path "/:ref" $ do
      i <- "ref"
      dispatch (RefR i)
    dispatch HomeR

app = run (App [] [] [] [] () update view) 
  where
    update _ () _ = pure

    view HomeR ws _ = Home.page ws Nothing
    view (RefR i) ws _ = Home.page ws (Just i)

data AppT
instance Theme AppT where
  theme c = void $ do
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

