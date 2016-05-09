# hpack: An alternative format for Haskell packages

## Examples

 * Given this [package.yaml](https://github.com/sol/hpack/blob/master/package.yaml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/master/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/master/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/master/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/master/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/master/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/master/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/master/base-orphans.cabal)

## Documentation

### Quick-reference

#### Top-level fields

| Hpack | Cabal | Default | Notes | Example |
| --- | --- | --- | --- | --- |
| `name` | `name` | | | |
| `version` | `version` | `0.0.0` | | |
| `synopsis` | `synopsis` | | | |
| `description` | `description` | | | |
| `category` | `category` | | | |
| `stability` | `stability` | | | |
| `homepage` | `homepage` | If `github` given, `<repo>#readme` | | |
| `bug-reports` | `bug-reports` | If `github` given, `<repo>/issues` | | |
| `author` | `author` | | May be a list | |
| `maintainer` | `maintainer` | | May be a list | |
| `copyright` | `copyright`          |                                    | May be a list         |
| `license` | `license` | | | |
| `license-file` | `license-file` | `LICENSE` if file exists | | |
| `tested-with` | `tested-with` | | | |
| | `build-type` | `Simple` | | |
| | `cabal-version` | `>= 1.10` or `>= 1.21` | `>= 1.21` if library component has `reexported-modules` field | |
| `extra-source-files` | `extra-source-files` | | Accepts glob patterns | |
| `data-files` | `data-files` | | Accepts glob patterns | |
| `github` | `source-repository head` | | Accepts `user/repo` or `user/repo/subdir` | `github: foo/bar`
| `git`    | `source-repository head` | | No effect if `github` given | `git: https://my.repo.com/foo` |
| `flags`  | `flag <name>` | | Map from flag name to flag (see **flags** section below) | |
| `library` | `library` | | See **library fields** section below | |
| `executables` | `executable <name>` | | Map from executable name to executable (see **executable fields** section below) | |
| `tests` | `test-suite <name>` | | Map from test name to test (see **test fields** section below) | |
| `benchmarks` | `benchmark <name>` | | Map from benchmark name to benchmark (see **benchmark fields** section below) | |

#### Global top-level fields

These fields are merged with all library, executable, test, and benchmark components.

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- | --- |
| `source-dirs` | `hs-source-dirs` | | |
| `default-extensions` | `default-extensions` | | |
| `other-extension` | `other-extensions` | | |
| `ghc-options` | `ghc-options` | | |
| `ghc-prof-options` | `ghc-prof-options` | | |
| `cpp-options` | `cpp-options` | | |
| `c-sources` | `c-sources` | | |
| `extra-lib-dirs` | `extra-lib-dirs` | | |
| `extra-libraries` | `extra-libraries` | | |
| `include-dirs` | `include-dirs` | | |
| `install-includes` | `install-includes` | | |
| `ld-options` | `ld-options` | | |
| `buildable` | `buildable` | | May be overridden by later stanza |
| `dependencies` | `build-depends` | | |
| `when` | | | Accepts a list of conditionals (see **conditionals** section below) |

#### Library fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `exposed` | `exposed` | | |
| `exposed-modules` | `exposed-modules` | All modules in `source-dirs` less `other-modules` | |
| `other-modules` | `other-modules` | All modules in `source-dirs` less `exposed-modules` | |
| `reexported-modules` | `reexported-modules` | | |
| | `default-language` | `Haskell2010` | |

#### Executable fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `main` | `main-is` | | |
| `other-modules` | `other-modules` | | |
| | `default-language` | `Haskell2010` | |

#### Test fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | `other-modules` | | |
| | `default-language` | `Haskell2010` | |

#### Benchmark fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | `other-modules` | | |
| | `default-language` | `Haskell2010` | |

#### Flags

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `description` | `description` | | Optional |
| `manual` | `manual` | | Required (unline Cabal) |
| `default` | `default` | | Required (unlike Cabal) |

#### Conditionals

Conditionals with no else branch:

- Must have a `condition` field
- May have any number of other fields

For example,

    condition: os(darwin)
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

    condition: flag(fast)
    then:
      ghc-options: -O2
    else:
      ghc-options: -O0

becomes

    if flag(fast)
      ghc-options: -O2
    else
      ghc-options: -O2


### File globbing

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

### Slides

 - Slides from my talk about `hpack` at the Singapore Haskell meetup:
   http://typeful.net/talks/hpack

## Vim integration

To run `hpack` automatically on modifications to `package.yaml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yaml silent !hpack --silent
```
