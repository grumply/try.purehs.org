let backend = ../config.dhall
      { name = "backend"
      , synopsis = "backend server"
      }
let deps =
      [ "base"
      , "pure"
      , "pure-elm"
      , "pure-txt-interpolate"
      , "pure-server"
      , "pure-websocket"
      , "shared"
      , "bytestring"
      , "containers"
      , "directory"
      , "filepath"
      , "process"
      , "text"
      , "hashable"
      ]
in
  backend //
    { dependencies = deps
    , library =
        { source-dirs = [ "src" ]
        , other-modules = [] : List Text
        }
    , executables =
        { backend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = [ "backend" ] # deps
          } 
        }
    }