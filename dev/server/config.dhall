let server = ../config.dhall
      { name = "server"
      , synopsis = "file server" 
      }

in
  server //
    { dependencies = 
        [ "base"
        , "wai"
        , "wai-app-static"
        , "warp"
        , "optparse-applicative"
        ]
    , executables = 
        { server = 
          { source-dirs = [ "src" ]
          , main = "Main.hs" 
          } 
        }
    }
