{-# language QuasiQuotes, NumDecimals #-}
module Main where

import Pure.Elm hiding (Left,Right)
import Pure.Data.Txt as Txt
import Pure.Data.Txt.Interpolate
import Pure.Server
import Pure.WebSocket as WS

import Shared

import Data.Hashable
import Data.Text.IO as Text

import Control.Exception
import Control.Monad
import Data.List as List
import System.Directory
import System.Exit
import System.IO
import System.Process
import System.Timeout
import Text.Read

import Data.Bits
import Data.Char
import Data.Word

main :: IO ()
main = inject body (server ()) >> hSetBuffering stdout NoBuffering >> sleep
  where
    sleep = forever (delay (Seconds 60 0))

server :: () -> View
server = Component $ \_self -> def
    { construct = return ()
    , render    = \_ _ -> Server "try.purehs.org" 8080 conn
    }

data Model = Model [Word64]
data Msg = Startup | Compiled Word64 | Cleanup

conn :: WebSocket -> View
conn = run (App [Startup] [] [Cleanup] (Model []) update view) 
  where
    update Startup ws mdl = do
      enact ws backendImpl
      activate ws
      pure mdl
    
    update Cleanup ws (Model ps) = do
      for_ ps $ \p -> do
        let d :: String
            d = [i|dist/static/builds/#{p}|]
        removeDirectoryRecursive [i|#{d}/Main.jsexe/|]
        removeFile [i|#{d}/Main.js_hi|]
        removeFile [i|#{d}/Main.js_o|]
        removeFile [i|#{d}/out.txt|]
        removeFile [i|#{d}/err.txt|]
      pure (Model [])

    update (Compiled p) ws (Model ps) = 
      pure (Model (p : ps))

    view _ _ = Null

backendImpl :: ( msgs ~ '[]
               , reqs ~ '[Compile,ReadModule]
               ) => Elm Msg 
                 => Endpoints msgs reqs msgs reqs
backendImpl = Endpoints backendAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleCompile <:> handleReadModule <:> WS.none

handleReadModule :: RequestHandler ReadModule
handleReadModule = respondWith $ \h -> do
  case readMaybe h of
    Just (n :: Int) -> do
      let f = [i|dist/static/builds/#{n}/Main.hs|]
      fe <- doesFileExist f
      if fe
        then Just <$> Text.readFile f
        else pure Nothing
    Nothing -> pure Nothing


-- Data.Hashable doesn't work across GHC and GHCJS, so we need a 
-- custom hash with a fixed-width integral type.
{-# INLINE fnv64 #-}
fnv64 :: Txt -> Word64
fnv64 = Txt.foldl' hash 0xcbf29ce484222325
  where
    {-# INLINE hash #-}
    hash :: Word64 -> Char -> Word64
    hash i c = 
      let i' = i `xor` fromIntegral (ord c) 
      in i' * 0x100000001b3

handleCompile :: Elm Msg => RequestHandler Compile
handleCompile = respondWith $ \(cnt,cache) -> do
  let h = abs (fnv64 cnt)
      d = [i|dist/static/builds/#{h}/|]
      exe = [i|#{d}/Main.jsexe/|]
      mdl = [i|#{d}/Main.hs|] 
      out = [i|#{d}/out.txt|]
      err = [i|#{d}/err.txt|]
      pragmas = List.intercalate " " $ fmap ("-X" <>)
        [ "AutoDeriveTypeable", "BangPatterns", "ConstraintKinds", "DataKinds"
        , "DefaultSignatures", "DeriveDataTypeable", "DeriveFoldable"
        , "DeriveFunctor", "DeriveGeneric", "DeriveTraversable", "DoAndIfThenElse"
        , "EmptyCase", "EmptyDataDecls", "ExistentialQuantification"
        , "FlexibleContexts", "FlexibleInstances", "FunctionalDependencies"
        , "GADTs", "GeneralizedNewtypeDeriving", "ImplicitParams", "InstanceSigs"
        , "KindSignatures", "LambdaCase", "MultiParamTypeClasses", "MultiWayIf"
        , "NamedFieldPuns", "OverloadedStrings", "PartialTypeSignatures"
        , "PatternGuards", "PatternSynonyms", "PolyKinds", "RankNTypes"
        , "RecordWildCards", "ScopedTypeVariables", "StandaloneDeriving"
        , "TupleSections", "TypeApplications", "TypeFamilies"
        , "TypeFamilyDependencies", "TypeOperators", "TypeSynonymInstances"
        , "ViewPatterns", "PostfixOperators", "JavaScriptFFI", "QuasiQuotes"
        , "DerivingVia"
        ]
  de <- doesDirectoryExist exe
  if de 
    then do
      fe <- doesFileExist [i|#{exe}/index.html|]
      if fe
        then pure $ Right (show h)
        else Left <$> Text.readFile err
    else do
      createDirectoryIfMissing True d
      m <- openFile mdl WriteMode 
      Text.hPutStr m cnt
      hClose m
      o <- openFile out WriteMode
      e <- openFile err WriteMode 
      handle @SomeException (\_ -> hClose o >> hClose e >> pure (Left "Unknown exception encountered during compilation.")) $ do
        (_,_,_,ph) <- createProcess 
          (shell [i|ghcjs --make -O -DGHCJS_BROWSER -dedupe #{mdl} #{pragmas} -XNoTemplateHaskell|])
            { std_out = UseHandle o 
            , std_err = UseHandle e
            , std_in  = NoStream
            }
        mec <- timeout 3e+7 (waitForProcess ph)
        hClose o
        hClose e
        unless cache $ command (Compiled h)
        case mec of
          Just ExitSuccess -> pure (Right $ show h)
          Nothing          -> pure (Left "Timeout: compilation took more than 30 seconds.")
          _                -> Left <$> Text.readFile err

