[![Hackage version](https://img.shields.io/hackage/v/hpack.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/hpack)
[![Stackage LTS version](https://www.stackage.org/package/hpack/badge/lts?label=Stackage)](https://www.stackage.org/package/hpack)
[![hpack on Stackage Nightly](https://stackage.org/package/hpack/badge/nightly)](https://stackage.org/nightly/package/hpack)

# hpack: A modern format for Haskell packages

Hpack is a format for Haskell packages.  It is a modern alternative to the
Cabal package format and follows different design principles.


## Design principles
The guiding design principles for Hpack are:

* Don't require the user to state the obvious, make sensible assumptions by
  default
* Give the user 100% control when needed
* Don't require the user to repeat things, facilitate [DRY](https://en.wikipedia.org/wiki/Don't_repeat_yourself)ness

## Tool integration

Hpack packages are described in a file named `package.yaml`.  Both
[`cabal2nix`](https://hackage.haskell.org/package/cabal2nix) and
[`stack`](https://haskellstack.org/) support `package.yaml` natively.  For
other build tools the `hpack` executable can be used to generate a `.cabal`
file from `package.yaml`.

## There is no user guide

There is reference documentation below, but introductory documentation is still
lacking. For the time being, take a look at the slides from my talk about Hpack
at the Singapore Haskell meetup: http://typeful.net/talks/hpack

## Examples

 * Given this [package.yaml](https://github.com/sol/hpack/blob/main/package.yaml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/main/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/main/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/main/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/main/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/main/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/main/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/main/base-orphans.cabal)

## Documentation

<!--ts-->
   * [hpack: A modern format for Haskell packages](#hpack-a-modern-format-for-haskell-packages)
      * [Design principles](#design-principles)
      * [Tool integration](#tool-integration)
      * [There is no user guide](#there-is-no-user-guide)
      * [Examples](#examples)
      * [Documentation](#documentation)
         * [Handling of Paths_ modules](#handling-of-paths_-modules)
            * [Modern behavior](#modern-behavior)
            * [Legacy behavior](#legacy-behavior)
         * [Quick-reference](#quick-reference)
            * [Top-level fields](#top-level-fields)
            * [cabal-version](#cabal-version)
            * [Defaults](#defaults)
            * [Custom setup](#custom-setup)
            * [Common fields](#common-fields)
            * [Library fields](#library-fields)
            * [Executable fields](#executable-fields)
            * [Test fields](#test-fields)
            * [Benchmark fields](#benchmark-fields)
            * [Flags](#flags)
            * [Dependencies](#dependencies)
            * [Conditionals](#conditionals)
         * [File globbing](#file-globbing)
         * [Passing things to Cabal verbatim](#passing-things-to-cabal-verbatim)
            * [Objects](#objects)
            * [Strings](#strings)
            * [Lists of objects and strings](#lists-of-objects-and-strings)
         * [Not repeating yourself](#not-repeating-yourself)
      * [The hpack executable](#the-hpack-executable)
      * [Vim integration](#vim-integration)
      * [Stack support](#stack-support)
      * [Binaries for use on Travis CI](#binaries-for-use-on-travis-ci)

<!-- Added by: sol, at: Mon Sep 18 11:40:17 AM +07 2023 -->

<!--te-->

### Handling of `Paths_` modules

Cabal generates a `Paths_` module for every package.  How exactly Hpack behaves
in regards to that module depends on the value of the `spec-version` field.

If the `spec-version` is explicitly specified and at least `0.36.0` the modern
behavior is used, otherwise Hpack falls back to the legacy behavior.

To use the modern behavior, require at least
```yaml
spec-version: 0.36.0
```
in your `package.yaml`.

#### Modern behavior

If you want to use the `Paths_` module for a component, you have to explicitly
specify it under `generated-other-modules`.

***Example:***

```yaml
library:
  source-dirs: src
  generated-other-modules: Paths_name # substitute name with the package name
```

#### Legacy behavior

For historic reasons Hpack adds the `Paths_` module to `other-modules` when
generating a `.cabal` file.

To prevent Hpack from adding the `Paths_` module to `other-modules` add the
following to `package.yaml`:

```yaml
library:
  when:
  - condition: false
    other-modules: Paths_name # substitute name with the package name
```

### Quick-reference

#### Top-level fields

| Hpack | Cabal | Default | Notes | Example | Since |
| --- | --- | --- | --- | --- | --- |
| `spec-version` |  | | The minimum version of `hpack` that is required to parse this package description.  | `spec-version: 0.30.0` | `0.30.0` |
| `name` | · | | | | |
| `version` | · | `0.0.0` | | | |
| `synopsis` | · | | | | |
| `description` | · | | | | |
| `category` | · | | | | |
| `stability` | · | | | | |
| `homepage` | · | If `github` given, `<repo>#readme` | | | |
| `bug-reports` | · | If `github` given, `<repo>/issues` | | | |
| `author` | · | | May be a list | | |
| `maintainer` | · | `author` | May be a list | | |
| `copyright` | · | | May be a list | |
| `license` | · | Inferred from `license-file` | Both [SPDX license expressions](https://spdx.org/licenses/) and traditional Cabal license identifiers are accepted. | `license: MIT` | SPDX: `0.29.0` |
| `license-file` | `license-file` or `license-files` | `LICENSE` if file exists | May be a list | | |
| `tested-with` | · | | May be a list (since `0.34.3`) | | |
| `build-type` | · | `Simple`, or `Custom` if `custom-setup` exists | Must be `Simple`, `Configure`, `Make`, or `Custom` | | |
| `extra-source-files` | · | | Accepts [glob patterns](#file-globbing) | | |
| `extra-doc-files` | · | | Accepts [glob patterns](#file-globbing) | | `0.21.2` |
| `data-files` | · | | Accepts [glob patterns](#file-globbing) | | |
| `data-dir` | · | | | | |
| `github` | `source-repository head` | | Accepts `owner/repo` or `owner/repo/subdir` | `github: foo/bar` |
| `git`    | `source-repository head` | | No effect if `github` given | `git: https://my.repo.com/foo` | |
| `custom-setup` | · | | See [Custom setup](#custom-setup) | | |
| `flags`  | `flag <name>` | | Map from flag name to flag (see [Flags](#flags)) | | |
| `library` | · | | See [Library fields](#library-fields) | | |
| `internal-libraries` | `library <name>` | | Map from internal library name to a dict of [library fields](#library-fields) and global top-level fields. | | `0.21.0` |
| `executables` | `executable <name>` | | Map from executable name to executable (see [Executable fields](#executable-fields)) | | |
| `executable` | `executable <package-name>` | | Shortcut for `executables: { package-name: ... }` | | `0.18.0` |
| `tests` | `test-suite <name>` | | Map from test name to test (see [Test fields](#test-fields)) | | |
| `benchmarks` | `benchmark <name>` | | Map from benchmark name to benchmark (see [Benchmark fields](#benchmark-fields)) | | |
| `defaults` | | | See [Defaults](#defaults), may be a list | | |

#### cabal-version

Hpack does not require you to specify a `cabal-version` manually.  When
generating a `.cabal` file, Hpack sets the `cabal-version` automatically based
on the features that are used.

If you want to override this behavior you can use `verbatim` to set
`cabal-version` manually, e.g.:

```yaml
verbatim:
  cabal-version: 2.2
```

#### Defaults

Hpack allows the inclusion of [common fields](#common-fields) from a file on
GitHub or a local file.

To use this feature a user must specify a GitHub repository, Git reference and
a path to a file within that repository; alternatively, a path to the local
file must be given.

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
| `github` | For github defaults. | Accepts `<owner>/<repo>` | `github: sol/hpack-template` |
| `ref` | | For github defaults. | `ref: 2017` |
| `path` | `.hpack/defaults.yaml` | For github defaults. A relative path to a file within the repository, path segments are separated by `/` and must not contain `:` and `\`. | `path: defaults.yaml` |
| `local` | | For local defaults. New in `0.26.0`. | |

Exactly one of `github` and `local` must be given in a `defaults` section.

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
`~/.hpack/defaults/<owner>/<repo>/<path>`.  Once downloaded, a file is reused
from the cache.  If the content on GitHub changes the file is not updated.  For
this reason it is recommended to only use tags as Git references.

 * If a defaults file has changed on GitHub and you want to use the latest
   version, then you have to delete that file from the cache manually.

 * If you want to prevent Hpack from accessing the network to download a
   defaults file, then you can achieve this by adding that file to the cache
   manually.

#### Custom setup

| Hpack | Cabal | Default | Notes | Example |
| --- | --- | --- | --- | --- |
| `dependencies` | `setup-depends` | | Implies `build-type: Custom` | |

#### Common fields

These fields can be specified top-level or on a per section basis; top-level
values are merged with per section values.

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `buildable` | · | | Per section takes precedence over top-level |
| `source-dirs` | `hs-source-dirs` | | |
| `default-extensions` | · | | |
| `language` | `default-language` | `Haskell2010` | Also accepts `Haskell98`, `GHC2021` or `GHC2024`. Per section takes precedence over top-level |
| `other-extensions` | · | | |
| `ghc-options` | · | | |
| `ghc-prof-options` | · | | |
| `ghc-shared-options` | · | | |
| `ghcjs-options` | · | | |
| `cpp-options` | · | | |
| `asm-options` | · | | |
| `asm-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `cc-options` | · | | |
| `c-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `cxx-options` | · | | |
| `cxx-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `js-sources` | · | | Accepts [glob patterns](#file-globbing) |
| `extra-lib-dirs` | · | | |
| `extra-libraries` | · | | |
| `include-dirs` | · | | |
| `install-includes` | · | | |
| `frameworks` | · | | |
| `extra-frameworks-dirs` | · | | |
| `ld-options` | · | | |
| `dependencies` | `build-depends` | | See [Dependencies](#dependencies) |
| `pkg-config-dependencies` | `pkgconfig-depends` | | |
| `build-tools` | [`build-tools`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-build-tools) and/or [`build-tool-depends`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-build-tool-depends) | | |
| `system-build-tools` | `build-tools` | | A set of system executables that have to be on the `PATH` to build this component |
| `when` | | | Accepts a list of conditionals (see [Conditionals](#conditionals)) |

**`build-tools`: A set of Haskell executables that are needed to build this component**

Each element consists of a *name* and an optional *version constraint*.

The name can be specified in two ways:

1. Qualified: `<package>:<executable>`
1. Unqualified: `<executable>`

A qualified name refers to an executable named `<executable>` from a
package named `<package>`.

An unqualified name either refers to an executables in the same package, or if
no such executable exists it is desugared to `<executable>:<executable>`.

`build-tools` can be specified as a list or a mapping.

Examples:
```yaml
build-tools:
  - alex
  - happy:happy
  - hspec-discover == 2.*
```
```
build-tools:
  alex: 3.2.*
  happy:happy: 1.19.*
  hspec-discover: 2.*
```

When generating a `.cabal` file each element of `build-tools` is either added
to `build-tools` or `build-tool-depends`.

If the name refers to one of `alex`, `c2hs`, `cpphs`, `greencard`, `haddock`,
`happy`, `hsc2hs` or `hscolour` then the element is added to `build-tools`,
otherwise it is added to `build-tool-depends`.

This is done to allow compatibility with a wider range of `Cabal` versions.

**Note:** Unlike `Cabal`, Hpack does not accept system executables as
`build-tools`.  Use `system-build-tools` if you need this.

#### Library fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `exposed` | · | | |
| `visibility` | · | | |
| `exposed-modules` | · | All modules in `source-dirs` less `other-modules` less any modules mentioned in `when` | |
| `generated-exposed-modules` | | | Added to `exposed-modules` and `autogen-modules`. Since `0.23.0`.
| `other-modules` | · | Outside conditionals: All modules in `source-dirs` less `exposed-modules` less any modules mentioned in `when`. Inside conditionals, and only if `exposed-modules` is not specified inside the conditional: All modules in `source-dirs` of the conditional less any modules mentioned in `when` of the conditional | |
| `generated-other-modules` | | | Added to `other-modules` and `autogen-modules`. Since `0.23.0`.
| `reexported-modules` | · | | |
| `signatures` | · | | |

#### Executable fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| `generated-other-modules` | | | Added to `other-modules` and `autogen-modules`. Since `0.23.0`.

#### Test fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| `generated-other-modules` | | | Added to `other-modules` and `autogen-modules`. Since `0.23.0`.

#### Benchmark fields

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| | `type` | `exitcode-stdio-1.0` | |
| `main` | `main-is` | | |
| `other-modules` | · | All modules in `source-dirs` less `main` less any modules mentioned in `when` | |
| `generated-other-modules` | | | Added to `other-modules` and `autogen-modules`. Since `0.23.0`.

#### Flags

| Hpack | Cabal | Default | Notes |
| --- | --- | --- | --- |
| `description` | · | | Optional |
| `manual` | · | | Required (unlike Cabal) |
| `default` | · | | Required (unlike Cabal) |

#### Dependencies

Dependencies can be specified as either a list or an object. These are
equivalent:

```
  dependencies:
    - base >= 4.10.1.0
    - containers >= 5.10
```

```
  dependencies:
    base: ">= 4.10.1.0"
    containers: ">= 5.10"
```

The individual dependencies can also be specified as an object:

```
  dependencies:
    - name: base
      version: ">= 4.10.1.0"
    - name: containers
```

You can use objects at both levels, or have a mix of valid ways to
specify the individual dependencies:

```
  dependencies:
    base:
      version: ">= 4.10.1.0"
    # If you don't give a version, it defaults to 'any version'.
    containers: {}
    transformers: ">= 0.5.5.0 && < 5.6"
```

Individual dependencies as objects are only supported from version
`0.31.0`.

When a dependency is specified as an object, you can use the `mixin`
field to control what modules from the dependency your program will
see and how its signatures are filled in:

```
  dependencies:
    # This gives you a shorter name to import from, and hides the other modules.
    - name: containers
      mixin:
        - (Data.Map.Lazy as Map)
    # This hides the System.IO.Unsafe module, and leaves the other modules unchanged.
    - name: base
      mixin:
        - hiding (System.IO.Unsafe)
    # This exposes only the listed modules - you won't be able to import the others!
    - name: lens
      mixin:
        - (Control.Lens, Data.Set.Lens, Data.Map.Lens as MapL)
    # This will rename the module, and expose the others.
    - name: transformers
      mixin:
        - hiding (Control.Monad.Trans.State.Lazy)
        - (Control.Monad.Trans.State.Lazy as State)
```

For more information, see the
[Cabal documentation](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-mixins).

Hint: you can hide the `Prelude` module from `base`, and then rename
an alternative prelude to `Prelude` so that it doesn't need to be
imported!

`mixin` was added in version `0.31.0`.

#### Conditionals

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

**Note:** Conditionals with `condition: false` are omitted from the generated
`.cabal` file.

### File globbing

At place where you can specify a list of files you can also use glob patterns.
Glob patterns and ordinary file names can be freely mixed, e.g.:

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

### Passing things to Cabal verbatim

(since `hpack-0.24.0`)

In cases where Hpack does not (yet!) support what you want to do, you can use
the `verbatim` field to pass things to Cabal verbatim.
It is recognized top-level, in sections, and in conditionals.

`verbatim` accepts an object or a string (or a list of objects and strings).

***Disclaimer:*** The content of `verbatim` fields are merged into the
generated `.cabal` file as a final step, after Hpack is done with most of its
work.  Before that final step Hpack does not look at any `verbatim` fields.
Consequently, the content of a `verbatim` field does not affect any other
fields that are populated by Hpack.  As an example, if you use `verbatim` to
override `hs-source-dirs`, the overridden information will not be used when
Hpack infers `exposed-modules` or `other-modules`.

#### Objects

When an object is used:

 - field values can be strings, numbers, booleans, or `null`
 - existing `.cabal` fields can be overridden
 - existing `.cabal` fields can be removed by overriding with `null`
 - additional `.cabal` fields can be added

 Example:

```yaml
tests:
  spec:
    main: Spec.hs
    source-dirs: test
    verbatim:
      type: detailed-0.9     # change type from exitcode-stdio-1.0
      default-language: null # remove default-language
```

#### Strings

When a string is used:

 - it will be added verbatim, indented to match the indentation of the surrounding context.
 - all existing `.cabal` fields are left untouched

Example:

```yaml
verbatim: |
  build-tool-depends:
      hspec-discover:hspec-discover == 2.*
```

#### Lists of objects and strings

You can combine the use of objects and strings to gain more fine-grained
control, e.g. you can remove an existing field with an object and then include
it with a string so that you have 100% control over the layout.


```yaml
verbatim:
  - build-depends: null
  - |
    -- let's use Cabal 5.0 dependency syntax
    build-depends:
      hspec: [2-3[
```

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

Fields that start with an underscore are ignored by `hpack`, so they can be
used to declare aliases:

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

## The hpack executable

If the `hpack` executable is on the `PATH`, to obtain help about its usage,
command `hpack --help`. In addition to its main use, `hpack` can also be used as
follows:

* `hpack --version` Output information about the version of `hpack` to the
  standard output channel, in the format `hpack version x.y.z`.
* `hpack --numeric-version` Output information about the version of `hpack` to
  the standard output channel, in the format `x.y.z`.
* `hpack --help` Output information about the usage of `hpack` to the standard
  error channel.
* `hpack list` (undocumented) Output the names of the package's exposed modules
  to the standard output channel, in the format of one name on each line.

In respect of its main use, `hpack` has the following optional flags:

* `--silent` Output no information other than error messages.
* `--canonical` By default, `hpack` takes into account aspects of the format of
  an existing Cabal file when generating a new Cabal file. Pass this flag to
  cause `hpack` to ignore the format of an existing Cabal file when generating a
  new one.
* `--force` or `-f` By default, `hpack` will not generate a Cabal file
  unnecessarily. Pass this flag to force the generation of a new Cabal file.
* `--[no-]hash` Enable/disable the inclusion of a SHA-256 hash of the other
  content of the generated Cabal file in the header comment added by `hpack` to
  the generated Cabal file. (default: disabled)
* `-` Output the generated Cabal file contents to the standard output channel.

By default, `hpack` will assume the package description in the Hpack format is
in file `package.yaml` in the current working directory. Alternatively, a
relative or absolute path to a file can be specified.

## Vim integration

To run `hpack` automatically on modifications to `package.yaml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yaml call Hpack()

function Hpack()
  let err = system('hpack ' . expand('%'))
  if v:shell_error
    echo err
  endif
endfunction
```

## Stack support

[Stack](https://haskellstack.org) has built-in support for Hpack.
If you are using Stack you can use `package.yaml` instead of a `.cabal` file.  No additional
steps are required.

## Binaries for use on CI

You can get binaries for use on CI with:

```
curl -sSL https://raw.githubusercontent.com/sol/hpack/main/get-hpack.sh | bash
hpack && git diff --exit-code
```

(both Linux and ~OS X~ are supported)
