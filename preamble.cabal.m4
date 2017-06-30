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
                     , Preamble.Stats
                     , Preamble.Trace
                     , Preamble.Types
                     , Preamble.Types.Alias
                     , Preamble.Types.Ctx
                     , Preamble.Types.Orphan
                     , Preamble.Types.Trans
  default-language:    Haskell2010
  ghc-options:         -Wall
  build-depends:       MonadRandom          >= 0.4.2
                     , aeson                >= 0.11
                     , base                 >= 4.8    && < 5
                     , basic-prelude        >= 0.5.2
                     , exceptions           >= 0.8.2
                     , fast-logger          >= 2.4
                     , lens                 >= 4.13
                     , monad-control        >= 1.0
                     , monad-logger         >= 0.3.17
                     , mtl                  >= 2.2.1
                     , network              >= 2.6.2
                     , resourcet            >= 1.1.7
                     , safe                 >= 0.3.9
                     , template-haskell     >= 2.10
                     , text                 >= 1.2.2
                     , text-manipulate      >= 0.2
                     , time                 >= 1.5
                     , transformers-base    >= 0.4.4
                     , unordered-containers >= 0.2.5
                     , uuid                 >= 1.3.11

executable shake-preamble
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , shakers
  default-language:    Haskell2010

source-repository head
  type:                git
  location:            git@github.com:swift-nav/preamble.git
