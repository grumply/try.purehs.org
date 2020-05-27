let dev = ../config.dhall
      { name = "dev"
      , synopsis = "dev environment" 
      }
in
  dev //
    { dependencies = 
        [ "base"
        , "pure-time"
        , "pure-txt"
        , "pure-txt-interpolate"
        , "fsnotify"
        , "directory"
        , "filepath"
        , "process"
        , "containers"
        , "Glob"
        ]
    , executables = 
        { dev = 
          { source-dirs = [ "src" ]
          , main = "Main.hs" 
          } 
        }
    }
