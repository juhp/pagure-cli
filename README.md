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
By default it uses the [src.fedoraproject.org](https://src.fedoraproject.org/)
Pagure server instance (aka Fedora dist-git).

- List or search for source packages (one can also filter by owner or committer):

```
$ pagure list emacs\*
emacs
:
```
(Note by default 'orphan' owned packages are excluded.)

- Count packages of a user:

```
$ pagure user --count mattdm
19
```

- List projects on pagure.io:

```
$ pagure list -s pagure.io \*
:
```

- There are more commands:

```
$ pagure --version
0.2
$ pagure --help
Pagure client

Usage: pagure [--version] COMMAND
  Simple pagure CLI

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list                     list projects
  user                     user repos
  branches                 list project branches
  issues                   list project issues
  users                    list users
  username                 fullname of user
  groups                   list groups
  git-url                  show project repo's git urls
```

## Installation

To build the latest release you will need cabal-install and ghc, or stack:

`stack install pagure-cli` or `cabal new-install pagure-cli`.

Or to build from the git source, run directly without the package name.

## Binaries

If you are using Fedora you can install the package from my
[copr repo](https://copr.fedorainfracloud.org/coprs/petersen/pagure-cli/).

## Contributions

Pagure rest API can be found on pagure servers: eg <https://pagure.io/api/0>.
There are still many unsupported commands and options.
Please open a ticket or PR to request adding more.

## Other clients
After writing the initial version I discovered that
Ricky Elrod (relrod) had made <https://github.com/fedora-infra/pagure-cli>.
