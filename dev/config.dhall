let authorName = "<Author>"

let authorEmail = "<Email>"

let extensions = 
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
      , "ViewPatterns", "TemplateHaskell"
      ] 

let PackageConfiguration =
      { name : Text
      , synopsis : Text
      }

let package = \(pkg : PackageConfiguration) ->
      { name = pkg.name
      , version = "0.1.0.0"
      , synopsis = pkg.synopsis
      , category = "Web"
      , author = authorName
      , email = authorEmail
      , license = "BSD3"
      , ghc-options = "-Wall -fno-warn-unused-do-bind -rtsopts -threaded"
      , ghcjs-options = "-threaded -dedupe -DGHCJS_BROWSER -DGHCJS_GC_INTERVAL=3000000"
      , default-extensions = extensions
      }

in
  package
