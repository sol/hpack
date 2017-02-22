# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]
- Allow `license-file` to be a list
- Accept input file on command-line (#106)

## [0.17.0] - 2017-01-30
### Added
- custom-setup section
  [#148](https://github.com/sol/hpack/pull/148)
- Support `!include` directives
  [#144](https://github.com/sol/hpack/pull/144)

## [0.16.0] - 2017-01-11
### Added
- Warn when `name` is missing [#109](https://github.com/sol/hpack/issues/109)
- Support globs in `c-sources`
  [#123](https://github.com/sol/hpack/pull/123)

### Changed
- Use binary I/O for cabal files avoiding problems with non-UTF-8 locales
  [#142](https://github.com/sol/hpack/pull/142)
  [#143](https://github.com/sol/hpack/pull/143)
- Fix rendering of `.` as directory (cabal syntax issue)
  [#125](https://github.com/sol/hpack/pull/125)
  [#119](https://github.com/sol/hpack/issues/119)
  [#67](https://github.com/sol/hpack/issues/67)

[Unreleased]: https://github.com/sol/hpack/compare/0.17.0...HEAD
[0.17.0]: https://github.com/sol/hpack/compare/0.16.0...0.17.0
[0.16.0]: https://github.com/sol/hpack/compare/0.15.0...0.16.0
