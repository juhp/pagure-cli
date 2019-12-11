# pagure-cli

[![Hackage](https://img.shields.io/hackage/v/pagure-cli.svg)](https://hackage.haskell.org/package/pagure-cli)
[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pagure-cli/badge/lts)](http://stackage.org/lts/package/pagure-cli)
[![Stackage Nightly](http://stackage.org/package/pagure-cli/badge/nightly)](http://stackage.org/nightly/package/pagure-cli)
[![Build status](https://secure.travis-ci.org/juhp/pagure-cli.svg)](https://travis-ci.org/juhp/pagure-cli)
[![Copr build](https://copr.fedorainfracloud.org/coprs/petersen/pagure-cli/package/pagure-cli/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/petersen/pagure-cli/)

A [pagure](https://docs.pagure.org/pagure/) client for querying
projects and users.

## Usage examples
It defaults to using the src.fedoraproject.org Pagure instance.

List or search for source packages (can also filter by owner):
```
$ pagure list emacs\*
emacs
:
```

List packages by user:
```
$ pagure user --count mattdm
19
```

List projects on pagure.io:
```
$ pagure list -s pagure.io \*
:
```

## Installation

To build from source you will need cabal-install and ghc, or stack.

`cabal install pagure-cli` or `stack install pagure-cli`

(or directly in the git source without the package name).
