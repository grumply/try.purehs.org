{-# language QuasiQuotes, NoMonomorphismRestriction, ImplicitParams, MultiWayIf, BlockArguments #-}
module Main where

import Dev

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \as -> do
  let 
    has = (`elem` as)
    v = has "--verbose"
    o = has "--optimize"

  defaultMain "app" $
    if | has "--ghcjs" -> 
         app "frontend" "frontend.project" v o frontend
       | otherwise -> concat
         [ app "backend" "backend.project" v o backend 
         ]

frontend :: PreApp => [Action]
frontend = 
  [ Restartable project $ first
    [ [i|app/shared/shared.cabal|]     |% full
    , [i|app/config.dhall|]            |% full
    , [i|app/frontend/config.dhall|]   |% full
    , [i|app/frontend/**/*.hs|]        |% partial
    -- order important
    , [i|app/frontend/**/*.hs|]        |* full
    , [i|app/shared/**/*.hs|]          |% partial
    ]
  ]
  where
    full :: App => IO ()
    full = run $ dhall $ hpack $ configureExe $ buildExe $ distribute

    partial :: App => IO ()
    partial = run $ buildExe $ distribute

shared :: PreApp => [Action]
shared = 
  [ Restartable project $ first
    [ [i|app/shared/config.dhall|] |% do
        run $ dhall $ hpack $ configureLib $ done
    , [i|app/shared/**/*.hs|] |* do
        run $ dhall $ hpack $ configureLib $ done
    ]
  ]

backend :: PreApp => [Action]
backend =
  [ Restartable project $ first
    [ [i|app/shared/shared.cabal|]     |% full
    , [i|app/backend/config.dhall|]    |% full
    , [i|app/config.dhall|]            |% full
    , [i|app/shared/**/*.hs|]          |% partial
    -- order important
    , [i|app/backend/**/*.hs|]         |* full
    , [i|app/backend/**/*.hs|]         |% partial
    ]
  ]
  where
    full :: App => IO ()
    full = run $ dhall $ hpack $ configureExe $ buildExe $ execute

    partial :: App => IO ()
    partial = run $ buildExe $ execute

run :: App => IO () -> IO ()
run x = clear >> status (Running [i|running|]) >> x

verbosity :: App => String
verbosity
  | verbose   = "--verbose" 
  | otherwise = ""

builddir :: App => String
builddir = [i|--builddir=.dist-newstyle/#{config}/#{project}|]

projectfile :: App => String
projectfile = [i|--project-file=#{config}|]

optimization :: App => String
optimization
  | optimize  = "--enable-optimization=2"
  | otherwise = "--disable-optimization"

cabal :: App => String -> String -> String
cabal cmd target = [i|cabal #{cmd} #{target}:#{project} #{optimization} #{builddir} #{projectfile} #{verbosity}|]

jsexe :: App => String
jsexe | optimize  = [i|.dist-newstyle/#{config}/#{project}/build/*/ghcjs-*/#{project}-*/x/#{project}/opt/build/#{project}/#{project}.jsexe|]
      | otherwise = [i|.dist-newstyle/#{config}/#{project}/build/*/ghcjs-*/#{project}-*/x/#{project}/noopt/build/#{project}/#{project}.jsexe|]

exe :: App => String
exe | optimize  = [i|.dist-newstyle/#{config}/#{project}/build/*/ghc-*/#{project}-*/x/#{project}/opt/build/#{project}/#{project}|]
    | otherwise = [i|.dist-newstyle/#{config}/#{project}/build/*/ghc-*/#{project}-*/x/#{project}/noopt/build/#{project}/#{project}|]

dhall :: App => IO () -> IO ()
dhall onSuccess = do
  clear
  ec <- spawn [i|dhall-to-yaml <<< ./app/#{project}/config.dhall > ./app/#{project}/.package.yaml|]
  case ec of
    ExitFailure _ -> status (Bad [i|dhall configuration failure|])
    ExitSuccess   -> onSuccess

hpack :: App => IO () -> IO ()
hpack onSuccess = do
  clear
  ec <- spawn [i|hpack --force ./app/#{project}/.package.yaml|]
  case ec of
    ExitFailure _ -> status (Bad [i|hpack failure|])
    ExitSuccess   -> onSuccess

configureExe :: App => IO () -> IO ()
configureExe onSuccess = do
  clear
  ec <- spawn (cabal "new-configure" "exe")
  case ec of
    ExitFailure _ -> status (Bad [i|build configuration failure|])
    ExitSuccess   -> onSuccess

configureLib :: App => IO () -> IO ()
configureLib onSuccess = do
  clear
  ec <- spawn (cabal "new-configure" "lib")
  case ec of
    ExitFailure _ -> status (Bad [i|build configuration failure|])
    ExitSuccess   -> onSuccess

buildExe :: App => IO () -> IO ()
buildExe onSuccess = do
  clear
  ec <- spawn (cabal "new-build" "exe")
  case ec of
    ExitFailure _ -> status (Bad [i|build failure|])
    ExitSuccess   -> onSuccess

buildLib :: App => IO () -> IO ()
buildLib onSuccess = do
  clear
  ec <- spawn (cabal "new-build" "lib")
  case ec of
    ExitFailure _ -> status (Bad [i|build failure|])
    ExitSuccess   -> onSuccess

distribute :: App => IO ()
distribute = do
  clear
  ec <- spawnSilent [i|(rm #{jsexe}/index.html || true) && cp #{jsexe}/* ./dist/|]
  case ec of
    ExitFailure _ -> status (Bad [i|distribute failure|])
    _ -> status (Good [i|success|])

execute :: App => IO ()
execute = do
  clear
  status (Good [i|executingj|])
  ec <- spawn [i|./#{exe}|]
  case ec of
    ExitFailure _ -> status (Bad [i|executable died|])
    _ -> status (Good [i|executable exit success|])

done :: App => IO ()
done = pure ()

type Project = (?project :: String)

withProject :: String -> (Project => a) -> a
withProject prj a = let ?project = prj in a

project :: Project => String
project = ?project

type Config = (?config :: String)

withConfig :: String -> (Config => a) -> a
withConfig cfg a = let ?config = cfg in a

config :: Config => String
config = ?config

type Verbose = (?verbose :: Bool)

withVerbosity :: Bool -> (Verbose => a) -> a
withVerbosity v a = let ?verbose = v in a

verbose :: Verbose => Bool
verbose = ?verbose

type Optimize = (?optimize :: Bool)

withOptimization :: Bool -> (Optimize => a) -> a
withOptimization o a = let ?optimize = o in a

optimize :: Optimize => Bool
optimize = ?optimize

type PreApp = (Optimize,Verbose,Project,Config)
type App = (Name,PreApp)

app :: String -> String -> Bool -> Bool -> (PreApp => a) -> a
app p c v o a = withProject p $ withConfig c $ withVerbosity v $ withOptimization o a
