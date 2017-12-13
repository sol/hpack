## next
  - Add `--numeric-version`

## Changes in 0.21.2
  - Fix a bug in module inference for conditionals (see #236)
  - Add support for `extra-doc-files`.
  - Add support for `pkg-config-dependencies`

## Changes in 0.21.1
  - Allow dependency constraints to be numbers (see #234)

## Changes in 0.21.0
  - Accept section-specific fields in conditionals (see #175, thanks to Scott
    Fleischman)
  - New section: `internal-libraries`, for Cabal 2's internal libraries (see #200).

## Changes in 0.20.0
  - Do not overwrite any existing cabal file if it has been modified manually

## Changes in 0.19.3
  - Add support for `frameworks` and `extra-frameworks-dirs`

## Changes in 0.19.2
 - Compatibility with `Glob >= 0.9.0`

## Changes in 0.19.1
  - Add `IsList` instance for `Dependencies`

## Changes in 0.19.0
  - Add Paths_* module to executables (see #195, for GHC 8.2.1 compatibility)
  - Allow specifying dependencies as a hash (see #198)

## Changes in 0.18.1
  - Output generated cabal file to `stdout` when `-` is given as a command-line
    option (see #113)
  - Recognize `.chs`, `.y`, `.ly` and `.x` as Haskell modules when inferring
    modules for

## Changes in 0.18.0
  - Make `executable` a shortcut of `executables: { package-name: ... }`
  - Add support for `ghcjs-options` and `js-sources` (see #161)
  - Allow `license-file` to be a list
  - Accept input file on command-line (see #106)
  - Add Paths_* when no modules are specified (see #86)

## Changes in 0.17.1
  - Do not descend into irrelevant directories when inferring modules (see #165)

## Changes in 0.17.0
  - Added custom-setup section
  - Add support for `!include` directives

## Changes in 0.16.0
  - Warn when `name` is missing
  - Support globs in `c-sources`
  - Use binary I/O for cabal files avoiding problems with non-UTF-8 locales
  - Fix rendering of `.` as directory (cabal syntax issue)
