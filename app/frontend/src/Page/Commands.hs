module Page.Commands where

import Pure.Elm

import Page.Themes
import Page.Utils

commands :: View
commands = 
  Div <| Themed @CommandsT |>
    [ Button <| Themed @EditButtonT . OnClick (\_ -> publish Edit) |> 
      [ "Edit" ] 
    , Button <| Themed @CompileButtonT . OnClick (\_ -> publish Compile) |> 
      [ "Compile" ]
    , Button <| Themed @VimButtonT . OnClick (\_ -> publish Vim) |>
      [ "Vim" ]
    ]