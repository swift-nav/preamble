# [preamble][preamble]

[![Build status][travis-img]][travis]

Yet another prelude. Builds on [basic-prelude][basic-prelude] with common
capabilities for monadic contexts and logging.

## Development

`preamble` has a shakefile/makefile to provide convience around building and testing:

    # build the project's libraries, executables, and tests
    $ ./Shakefile.hs build-tests-error
    
    # test the project
    $ ./Shakefile.hs tests-error
    
    # start an interpreter with the project's libraries, executables, and tests loaded
    $ ./Shakefile.hs ghci-tests
    
    # clean the project
    $ ./Shakefile.hs clean
    
    # lint the project source code
    $ ./Shakefile.hs lint
    
    # format the project source code
    $ ./Shakefile.hs format

## Dependencies

To build and test `preamble`, the following dependencies may be required:

+ [stack][stack]


[preamble]:      https://github.com/swift-nav/preamble
[hackage]:       https://hackage.haskell.org/package/preamble
[hackage-img]:   https://img.shields.io/hackage/v/preamble.svg?style=flat
[travis]:        https://travis-ci.org/swift-nav/preamble
[travis-img]:    https://img.shields.io/travis/swift-nav/preamble/master.svg?style=flat
[deps]:          http://packdeps.haskellers.com/feed?needle=preamble
[deps-img]:      https://img.shields.io/hackage-deps/v/preamble.svg?style=flat
[basic-prelude]: https://github.com/snoyberg/basic-prelude
[stack]:         https://docs.haskellstack.org/en/stable/README/#how-to-install
