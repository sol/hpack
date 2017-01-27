# hpack: An alternative format for Haskell packages

## Motivation

hpack makes the following improvements over the standard cabal file
format:

* YAML syntax is more ubiquitous than cabal syntax and a bit easier to
  write. It is also very easy to parse from other languages.
* `exposed-modules`/`other-modules` are set automatically. In a
  standard cabal file these are easy to forget when new modules are
  added and will result in obscure linker errors or worse.
* Support for glob patterns for file fields like `extra-source-files`
  and `data-files`.
* Automatically sets a sensible `cabal-version`.
* Automatically sets a `license-file` field if a `LICENSE` file exists.
* For the common case of hosting on github, `homepage`, `bug-reports`, and
  `source-repository` can be set automatically.
* Some fields like `dependencies` can be set at the top level and
  merged with the dependencies list of each compile target.
* Automatically supported by `stack`.

## Examples

 * Given this [package.yaml](https://github.com/sol/hpack/blob/master/package.yaml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/master/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/master/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/master/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/master/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/master/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/master/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/master/base-orphans.cabal)

## Documentation

### Getting started

One easy way of getting started is to use
[hpack-convert](http://hackage.haskell.org/package/hpack-convert) to convert an
existing cabal file into a `package.yaml`.

### Quick-reference

#### Top-level fields

| Hpack | Cabal | Default | Notes | Example |
| --- | --- | --- | --- | --- |
| `name` | · | | | |
| `version` | · | `0.0.0` | | |
| `synopsis` | · | | | |
| `description` | · | | | |
| `category` | · | | | |
| `stability` | · | | | |
| `homepage` | · | If `github` given, `<repo>#readme` | | |
| `bug-reports` | · | If `github` given, `<repo>/issues` | | |
| `author` | · | | May be a list | |
| `maintainer` | · | | May be a list | |
| `copyright` | · | | May be a list |
| `license` | · | | | |
| `license-file` | · | `LICENSE` if file exists | | |
| `tested-with` | · | | | |
| `build-type` | · | `Simple`, or `Custom` if `custom-setup` section exists | Must be `Simple`, `Configure`, `Make`, or `Custom` | |
| | `cabal-version` | `>= 1.10` or `>= 1.21` | `>= 1.21` if library component has `reexported-modules` field | |
| `extra-source-files` | · | | Accepts [glob patterns](#file-globbing) | |
| `data-files` | · | | Accepts [glob patterns](#file-globbing) | |
| `github` | `source-repository head` | | Accepts `user/repo` or `user/repo/subdir` | `github: foo/bar`
| `git`    | `source-repository head` | | No effect if `github` given | `git: https://my.repo.com/foo` |
| `custom-setup` | · | | See [Custom setup](#custom-setup) | |
| `flags`  | `flag <name>` | | Map from flag name to flag (see [Flags](#flags)) | |
| `library` | · | | See [Library fields](#library-fields) | |
| `executables` | `executable <name>` | | Map from executable name to executable (see [Executable fields](#executable-fields)) | |
| `tests` | `test-suite <name>` | | Map from test name to test (see [Test fields](#test-fields)) | |
| `benchmarks` | `benchmark <name>` | | Map from benchmark name to benchmark (see [Benchmark fields](#benchmark-fields)) | |

#### <a name="custom-setup"></a>Custom setup

| Hpack | Cabal | Default | Notes | Example |
| --- | --- | --- | --- | --- |
| `dependencies` | `setup-depends` | | Implies `build-type: Custom` | |

#### Global top-level fields

These fields are merged with all library, executable, test, and benchmark components.

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `source-dirs` | `hs-source-dirs` | | |
| `default-extensions` | · | | |
| `other-extensions` | · | | |
| `ghc-options` | · | | |
| `ghc-prof-options` | · | | |
| `cpp-options` | · | | |
| `cc-options` | · | | |
| `c-sources` | · | | |
| `extra-lib-dirs` | · | | |
| `extra-libraries` | · | | |
| `include-dirs` | · | | |
| `install-includes` | · | | |
| `ld-options` | · | | |
| `buildable` | · | | May be overridden by later stanza |
| `dependencies` | `build-depends` | | |
| `build-tools` | · | | |
| `when` | | | Accepts a list of conditionals (see [Conditionals](#conditionals)) |

#### <a name="library-fields"></a>Library fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `exposed` | · | | |
| `exposed-modules` | · | All modules in `source-dirs` less `other-modules` | |
| `other-modules` | · | All modules in `source-dirs` less `exposed-modules` | |
| `reexported-modules` | · | | |
| | `default-language` | `Haskell2010` | |

#### <a name="executable-fields"></a>Executable fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `main` | `main-is` | | |
| `other-modules` | · | | |
| | `default-language` | `Haskell2010` | |

#### <a name="test-fields"></a>Test fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | | |
| | `default-language` | `Haskell2010` | |

#### <a name="benchmark-fields"></a>Benchmark fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | | |
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

[yaml-anchor]: http://yaml.org/spec/1.1/#anchor/syntax
[yaml-alias]: http://yaml.org/spec/1.1/#alias/syntax
[yaml-merge]: http://yaml.org/type/merge.html

### Slides

 - Slides from my talk about `hpack` at the Singapore Haskell meetup:
   http://typeful.net/talks/hpack

## Vim integration

To run `hpack` automatically on modifications to `package.yaml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yaml silent !hpack --silent
```
