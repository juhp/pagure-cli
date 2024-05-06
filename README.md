# pagure-cli

[![Hackage](https://img.shields.io/hackage/v/pagure-cli.svg)](https://hackage.haskell.org/package/pagure-cli)
[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pagure-cli/badge/lts)](http://stackage.org/lts/package/pagure-cli)
[![Stackage Nightly](http://stackage.org/package/pagure-cli/badge/nightly)](http://stackage.org/nightly/package/pagure-cli)

A [pagure](https://docs.pagure.org/pagure/) client for querying
projects and users.

## Usage
By default it uses the [src.fedoraproject.org](https://src.fedoraproject.org/)
Pagure server instance (aka Fedora dist-git), but this can be overriden
by the `--server` option.

## List/search for repos

```
$ pagure list emacs\*
emacs
:
```
Note by default 'orphan' owned packages are excluded.

One can also filter by owner or committer.

## List user's projects

Count projects of a user:
```
$ pagure user -s pagure.io --count mattdm
93
```

## List groups:

```
$ pagure groups
:
```

## More commands

- `branches` lists the git branches of a repo

- `issues` to query and list issues

- `users` to list usernames of users

- `username` shows the fullname of a user

- `git-url` shows the git urls for a repo

## Help

Use `--help` to get help on individual commands:

`$ pagure --version`

```
0.2.2
```


`$ pagure list --help`

```
Usage: pagure list [-s|--server SERVER] [-c|--count] [(-j|--json) | (-y|--yaml)]
                   [(-F|--only-forks) | (-f|--include-forks)]
                   [-n|--namespace NAMESPACE]
                   [(-o|--owner OWNER) | (-u|--username USERNAME)] [PATTERN]

  list projects

Available options:
  -s,--server SERVER       Pagure server
  -c,--count               Show number only
  -j,--json                Output JSON
  -y,--yaml                YAML output
  -F,--only-forks          Only list forks
  -f,--include-forks       Include forks [default: ignore forks]
  -n,--namespace NAMESPACE Specify project repo namespace
  -o,--owner OWNER         Projects with certain owner
  -u,--username USERNAME   Projects to which username can commit
  -h,--help                Show this help text
```

## Installation

To build the latest release you will need cabal-install and ghc, or stack:

`stack install pagure-cli` or `cabal install pagure-cli`.

Or to build from the git source, run the commands without the package name.

## Binaries

pagure-cli is packaged in Fedora and EPEL 9: <https://src.fedoraproject.org/rpms/pagure-cli>

## Contributions

Pagure rest API can be found on pagure servers: eg <https://pagure.io/api/0>.
There are still many unsupported commands and options.
Please open a ticket or PR to request adding more.

Also part of this client uses the simple
[pagure-hs](https://hackage.haskell.org/package/pagure) bindings.

## Other pagure clients
After writing the initial version I discovered that
Ricky Elrod (relrod) had made <https://github.com/fedora-infra/pagure-cli>.

I discovered a client in Rust <https://pagure.io/ironthree/pagure-rs> by decathorpe.
