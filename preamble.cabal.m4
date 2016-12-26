name:                  preamble
version:               VERSION
synopsis:              Yet another prelude.
description:           A prelude built on basic-prelude.
homepage:              https://github.com/swift-nav/preamble
license:               MIT
license-file:          LICENSE
author:                Swift Navigation Inc.
maintainer:            Mark Fine <dev@swiftnav.com>
copyright:             Copyright (C) 2016 Swift Navigation, Inc.
category:              Prelude
build-type:            Simple
cabal-version:         >= 1.22

library
  hs-source-dirs:      src
  exposed-modules:     Preamble
  other-modules:       Preamble.Aeson
                     , Preamble.Ctx
                     , Preamble.Lens
                     , Preamble.Prelude
                     , Preamble.Trace
                     , Preamble.Types
                     , Preamble.Types.Alias
                     , Preamble.Types.Ctx
                     , Preamble.Types.Trans
  default-language:    Haskell2010
  ghc-options:         -Wall
  build-depends:       aeson
                     , base >= 4.8 && < 4.9
                     , basic-prelude
                     , exceptions
                     , fast-logger
                     , lens
                     , monad-control
                     , monad-logger
                     , mtl
                     , resourcet
                     , safe
                     , template-haskell
                     , text
                     , text-manipulate
                     , time
                     , transformers-base
                     , unordered-containers
                     , uuid

executable shake-preamble
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base >= 4.8 && < 4.9
                     , shakers
  default-language:    Haskell2010

source-repository head
  type:                git
  location:            git@github.com:swift-nav/preamble.git
