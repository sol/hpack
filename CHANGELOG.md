## Change in 0.18.0
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
