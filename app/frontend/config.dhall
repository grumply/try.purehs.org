let frontend = ../config.dhall
      { name = "frontend"
      , synopsis = "frontend client" 
      }
let deps = 
      [ "base"
      , "pure"
      , "pure-lifted"
      , "pure-elm"
      , "pure-router"
      , "pure-txt"
      , "pure-websocket"
      , "shared"
      ]
in
  frontend //
    { dependencies = deps
    , library =
        { source-dirs = [ "src" ]
        , other-modules = [] : List Text
        }
    , executables =
        { frontend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = [ "frontend" ] # deps
          } 
        }
    }
