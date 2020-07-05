{-# language QuasiQuotes #-}
module Main where

import Pure.Elm.Application
import Pure.WebSocket

import System.IO

import Prelude hiding (or)
import Data.Int

import qualified Page
import Page.Themes (AppT())

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- clientWS "204.48.20.19" 8080
  inject body (app ws)

data Route = R (Maybe String)

instance Routes Route where
  home = R Nothing
  routes = do
    path "/:ref" $ do
      i <- "ref"
      case i of
        "" -> dispatch (R Nothing)
        _  -> dispatch (R (Just i))
    dispatch home

app = run (App [] [] [] [] () update view) 
  where
    update _ () _ = pure
    view (R h) ws _ = Div <| Themed @AppT |> [ Page.page ws h ]

