# hpack: An alternative format for Haskell packages

The Hpack package format is based on [YAML](http://yaml.org/).  Packages
are described in a file named `package.yaml`.

Hpack is designed to remove redundancies from your package descriptions.

There is reference documentation below, but introductory documentation is still
lacking. For the time being, take a look at the slides from my talk about Hpack at the Singapore Haskell
meetup: http://typeful.net/talks/hpack

## Examples

 * Given this [package.yaml](https://github.com/sol/hpack/blob/master/package.yaml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/master/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/master/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/master/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/master/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/master/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/master/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/master/base-orphans.cabal)

## Documentation

### Quick-reference

#### Top-level fields

| Hpack | Cabal | Default | Notes | Example | Since |
| --- | --- | --- | --- | --- | --- |
| `name` | · | | | | |
| `version` | · | `0.0.0` | | | |
| `synopsis` | · | | | | |
| `description` | · | | | | |
| `category` | · | | | | |
| `stability` | · | | | | |
| `homepage` | · | If `github` given, `<repo>#readme` | | | |
| `bug-reports` | · | If `github` given, `<repo>/issues` | | | |
| `author` | · | | May be a list | | |
| `maintainer` | · | | May be a list | | |
| `copyright` | · | | May be a list | |
| `license` | · | | | | |
| `license-file` | `license-file` or `license-files` | `LICENSE` if file exists | May be a list | | |
| `tested-with` | · | | | | |
| `build-type` | · | `Simple`, or `Custom` if `custom-setup` exists | Must be `Simple`, `Configure`, `Make`, or `Custom` | | |
| `extra-source-files` | · | | Accepts [glob patterns](#file-globbing) | | |
| `extra-doc-files` | · | | Accepts [glob patterns](#file-globbing) | | `0.21.2` |
| `data-files` | · | | Accepts [glob patterns](#file-globbing) | | |
| `github` | `source-repository head` | | Accepts `user/repo` or `user/repo/subdir` | `github: foo/bar` |
| `git`    | `source-repository head` | | No effect if `github` given | `git: https://my.repo.com/foo` | |
| `custom-setup` | · | | See [Custom setup](#custom-setup) | | |
| `flags`  | `flag <name>` | | Map from flag name to flag (see [Flags](#flags)) | | |
| `library` | · | | See [Library fields](#library-fields) | | |
| `internal-libraries` | `library <name>` | | Map from internal library name to a dict of [library fields](#library-fields) and global top-level fields. | | `0.21.0` |
| `executables` | `executable <name>` | | Map from executable name to executable (see [Executable fields](#executable-fields)) | | |
| `executable` | `executable <package-name>` | | Shortcut for `executables: { package-name: ... }` | | `0.18.0` |
| `tests` | `test-suite <name>` | | Map from test name to test (see [Test fields](#test-fields)) | | |
| `benchmarks` | `benchmark <name>` | | Map from benchmark name to benchmark (see [Benchmark fields](#benchmark-fields)) | | |
| `defaults` | | | See [Defaults](#defaults) | | |

**Note:** Hpack does not require you to specify a `cabal-version` manually.  When
generating a `.cabal` file, Hpack sets the `cabal-version` automatically based
on the features that are used.

#### <a name="defaults"></a>Defaults

Hpack allows the inclusion of [common fields](#common-fields) from a file on
GitHub.

To use this feature a user must specify a GitHub repository, Git reference and
a path to a file within that repository.

Example:

```yaml
defaults:
  github: sol/hpack-template
  ref: 2017
  path: defaults.yaml
```

This will include all common fields from
https://github.com/sol/hpack-template/blob/2017/defaults.yaml into the package
specification.

| Field | Default | Notes | Example |
| ----- | ------- | ----- | ------- |
| `github` | | Accepts `<user>/<repo>` | `github: sol/hpack-template` |
| `ref` | | | `ref: 2017` |
| `path` | `.hpack/defaults.yaml` | A relative path to a file within the repository, path segments are separated by `/` and must not contain `:` and `\`. | `path: defaults.yaml` |

Hpack supports shorthand syntax for specifying `github` and `ref` as a string:

```yaml
defaults: sol/hpack-template@2017
```

This is equivalent to:

```yaml
defaults:
  github: sol/hpack-template
  ref: 2017
```

**Note:** Hpack caches downloaded files under
`~/.hpack/defaults/<user>/<repo>/<path>`.  Once downloaded, a file is reused
from the cache.  If the content on GitHub changes the file is not updated.  For
this reason it is recommended to only use tags as Git references.

 * If a defaults file has changed on GitHub and you want to use the latest
   version, then you have to delete that file from the cache manually.

 * If you want to prevent Hpack from accessing the network to download a
   defaults file, then you can achieve this by adding that file to the cache
   manually.

#### <a name="custom-setup"></a>Custom setup

| Hpack | Cabal | Default | Notes | Example |
| --- | --- | --- | --- | --- |
| `dependencies` | `setup-depends` | | Implies `build-type: Custom` | |

#### <a name="common-fields">Common fields

These fields can be specified top-level or on a per section basis; top-level
values are merged with per section values.

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `buildable` | · | | Per section takes precedence over top-level |
| `source-dirs` | `hs-source-dirs` | | |
| `default-extensions` | · | | |
| `other-extensions` | · | | |
| `ghc-options` | · | | |
| `ghc-prof-options` | · | | |
| `ghcjs-options` | · | | |
| `cpp-options` | · | | |
| `cc-options` | · | | |
| `c-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `js-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `extra-lib-dirs` | · | | |
| `extra-libraries` | · | | |
| `include-dirs` | · | | |
| `install-includes` | · | | |
| `frameworks` | · | | |
| `extra-frameworks-dirs` | · | | |
| `ld-options` | · | | |
| `dependencies` | `build-depends` | | |
| `pkg-config-dependencies` | `pkgconfig-depends` | | |
| `build-tools` | · | | |
| `when` | | | Accepts a list of conditionals (see [Conditionals](#conditionals)) |

#### <a name="library-fields"></a>Library fields

| Hpack | Cabal | Default | Default inside conditional |
| --- | --- | --- | --- |
| `exposed` | · | | |
| `exposed-modules` | · | All modules in `source-dirs` less `other-modules` less any modules mentioned in `when` | |
| `other-modules` | · | All modules in `source-dirs` less `exposed-modules` less any modules mentioned in `when` | Only if `exposed-modules` is not specified inside the conditional: All modules in `source-dirs` of the conditional less any modules mentioned in `when` of the conditional |
| `reexported-modules` | · | | |
| `signatures` | · | | |
| | `default-language` | `Haskell2010` | |

#### <a name="executable-fields"></a>Executable fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| | `default-language` | `Haskell2010` | |

#### <a name="test-fields"></a>Test fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| | `default-language` | `Haskell2010` | |

#### <a name="benchmark-fields"></a>Benchmark fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| | `default-language` | `Haskell2010` | |

#### <a name="flags"></a>Flags

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `description` | · | | Optional |
| `manual` | · | | Required (unlike Cabal) |
| `default` | · | | Required (unlike Cabal) |

#### <a name="conditionals"></a> Conditionals

Conditionals with no else branch:

- Must have a `condition` field
- May have any number of other fields

For example,

    when:
      - condition: os(darwin)
        extra-lib-dirs: lib/darwin

becomes

    if os(darwin)
      extra-lib-dirs:
        lib/darwin

Conditionals with an else branch:

- Must have a `condition` field
- Must have a `then` field, itself an object containing any number of other fields
- Must have a `else` field, itself an object containing any number of other fields
- All other top-level fields are ignored

For example,

    when:
      - condition: flag(fast)
        then:
          ghc-options: -O2
        else:
          ghc-options: -O0

becomes

    if flag(fast)
      ghc-options: -O2
    else
      ghc-options: -O0


### <a name="file-globbing"></a>File globbing

At place where you can specify a list of files you can also use glob patterns.
Glob patters and ordinary file names can be freely mixed, e.g.:

```yaml
extra-source-files:
  - static/*.js
  - static/site.css
```

Glob patterns are expanded according to the following rules:

 - `?` and `*` are expanded according to POSIX (they match arbitrary
   characters, except for directory separators)
 - `**` is expanded in a `zsh`-like fashion (matching across directory
   separators)
 - `?`, `*` and `**` do not match a `.` at the beginning of a file/directory

### Not repeating yourself

It is possible to use YAML [anchors][yaml-anchor] (`&`), [aliases][yaml-alias]
(`*`) and [merge keys][yaml-merge] (`<<`) to define fields and reference them
later.

[yaml-anchor]: http://yaml.org/spec/1.1/#anchor/syntax
[yaml-alias]: http://yaml.org/spec/1.1/#alias/syntax
[yaml-merge]: http://yaml.org/type/merge.html

```yaml
executables:
  my-exe-1: &my-exe
    main: my-exe-1.hs
    dependencies: [base, my-lib]
    ghc-options: [-threaded]
  my-exe-2:
    <<: *my-exe
    main: my-exe-2.hs
```

Warnings for unknown fields will not be emitted for top-level fields starting
with an underscore, so you can declare global aliases too:

```yaml
_exe-ghc-options: &exe-ghc-options
  - -threaded
  - -rtsopts

executables:
  my-exe-1:
    ghc-options: *exe-ghc-options
```

It is also possible to use the `!include` directive:

```yaml
# ...

tests:
  hlint: !include "../common/hlint.yaml"
```

`hlint.yaml`:

```yaml
source-dirs: test
main: hlint.hs
dependencies: [base, hlint]
```

This can also be used to provide entire libraries of snippets:

```yaml
_common/lib: !include "../common/lib.yaml"

name: example1
version: '0.1.0.0'
synopsis: Example
<<: *legal

<<: *defaults

library:
  source-dirs: src

tests:
  hlint: *test_hlint
```

lib.yaml:

```yaml
- &legal
  maintainer: Some One <someone@example.com>
  copyright: (c) 2017 Some One
  license: BSD3

- &defaults
  dependencies:
    - base
    - containers
  ghc-options:
    - -Wall
    - -Werror

- &test_hlint
  source-dirs: test
  main: hlint.hs
  dependencies: [hlint]
```

## Vim integration

To run `hpack` automatically on modifications to `package.yaml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yaml silent !hpack --silent
```

## Stack support

[Stack](https://haskellstack.org) has built-in support for Hpack.
If you are using Stack you can use `package.yaml` instead of a `.cabal` file.  No additional
steps are required.
