# hpack: An alternative format for Haskell packages

## Examples

 * Given this [package.yaml](https://github.com/sol/hpack/blob/master/package.yaml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/master/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/master/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/master/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/master/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/master/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/master/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/master/base-orphans.cabal)

## Documentation

Documentation is mostly missing so far, sorry for that!

 - Slides from my talk about `hpack` at the Singapore Haskell meetup:
   http://typeful.net/talks/hpack

### File globing

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

## Vim integration

To run `hpack` automatically on modifications to `package.yaml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yaml silent !hpack > /dev/null
```
