# pagure

[![Hackage](https://img.shields.io/hackage/v/pagure-cli.svg)](https://hackage.haskell.org/package/pagure-cli)
[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pagure-cli/badge/lts)](http://stackage.org/lts/package/pagure-cli)
[![Stackage Nightly](http://stackage.org/package/pagure-cli/badge/nightly)](http://stackage.org/nightly/package/pagure-cli)
[![Build status](https://secure.travis-ci.org/juhp/pagure-cli.svg)](https://travis-ci.org/juhp/pagure-cli)

Pagure client

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
