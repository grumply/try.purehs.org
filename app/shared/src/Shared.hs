{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Shared where

import Pure
import Pure.WebSocket as WS

import Data.Int

mkRequest "Compile" [t|Txt -> Either Txt String|]
mkRequest "ReadModule" [t|String -> Maybe Txt|]

backendAPI :: FullAPI '[] '[Compile,ReadModule]
backendAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = compile <:> readModule <:> WS.none
