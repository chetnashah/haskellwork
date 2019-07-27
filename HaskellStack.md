
The primary stack design point is reproducible builds.
To make this a simple process, stack uses curated package sets called `snapshots`.

### resolver


### Dirs managed by stack

will not make changes outside of specific stack directories. stack-built files generally go in either the stack root directory (default `~/.stack` or, on Windows, `%LOCALAPPDATA%\Programs\stack`) or `./.stack-work` directories local to each project

### New Project

`stack new my-project`
Creates following:
1. Setup.hs
2. stack.yaml
3. my-project.cabal
4. src/Main.hs

### `package.yaml`

#### dependencies
dependencies that are part of the package-set 
decided by `resolver` should go here. If
a package is not a part of snapshot/package-set
pointed by resolver, then it should go in `extra-deps`.


### `stack.yaml`

Tells stack which version of GHC and your dependencies
to use.

#### resolver
Part of `stack.yaml` file in project, specifies resolver to the snapshot which your package will be built against.

#### packages

A list of packages that are part of your local project. These are specified via paths to local directories. The paths are considered relative to the directory containing the stack.yaml file. For example, if your stack.yaml is located at /foo/bar/stack.yaml, and you have:
```yaml
packages:
- hello
- there/world
```
Your configuration means "I have packages in `/foo/bar/hello` and `/foo/bar/there/world`.

If these packages should be treated as dependencies instead, specify them in extra-deps

#### extra-deps

This field allows you to specify extra dependencies on top of what is defined in your snapshot (specified in the resolver field mentioned above). These dependencies may either come from a local file path or a Pantry package location.

For the `local file path` case, the same relative path rules as apply to packages apply.

`Pantry package locations` allow you to include dependencies from three different kinds of sources:

1. Hackage
2. Archives (tarballs or zip files, either local or over HTTP(S))
3. Git or Mercurial repositories


### `my-project.cabal`

This is like `package.json` of the project.
Important fields are `hs-source-dirs`, `main-is` which points to main file

### `src/Main.hs`
This is the file that is entry point. and the filename is
the value present in `main-is` mentioned above.

### Building executable

`stack build`
`stack exec my-project`

### `stack install`

The install command does precisely one thing in addition to the build command: it copies any generated executables to the local bin path which is `$HOME/local/.bin` on unix systems and
`%APPDATA%\local\bin` on windows systems.

### Stackage

Stable source of Haskell packages.
It is a curated set of packages that work well together, similar to how a Debian release is a curated snapshot of most of the open-source software in the world.
A Stackage snapshot includes pinned package versions from `Hackage`(index of all Haskell packages of all versions).
e.g. LTS haskell 9.21 from stackage pins all working packages with ghc compiler 8.0.2
All stackage LTS versions can be found at : https://www.stackage.org/

Here is the existing list:

LTS 13.29 for ghc-8.6.5, published a week ago
LTS 13.19 for ghc-8.6.4, published 3 months ago
LTS 13.11 for ghc-8.6.3, published 5 months ago
LTS 12.26 for ghc-8.4.4, published 7 months ago
LTS 12.14 for ghc-8.4.3, published 9 months ago
LTS 11.22 for ghc-8.2.2, published 11 months ago
LTS 9.21 for ghc-8.0.2, published a year ago
LTS 7.24 for ghc-8.0.1, published 2 years ago
LTS 6.35 for ghc-7.10.3, published 2 years ago
LTS 3.22 for ghc-7.10.2, published 4 years ago
LTS 2.22 for ghc-7.8.4, published 4 years ago
LTS 0.7 for ghc-7.8.3, published 5 years ago



