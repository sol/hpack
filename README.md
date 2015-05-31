# hpack: An alternative format for Haskell packages

## Examples

 * Given this [package.yml](https://github.com/sol/hpack/blob/master/package.yml) running `hpack` will generate [hpack.cabal](https://github.com/sol/hpack/blob/master/hpack.cabal)
 * Given this [package.yaml](https://github.com/zalora/getopt-generics/blob/master/package.yaml) running `hpack` will generate [getopt-generics.cabal](https://github.com/zalora/getopt-generics/blob/master/getopt-generics.cabal)
 * Given this [package.yaml](https://github.com/hspec/sensei/blob/master/package.yaml) running `hpack` will generate [sensei.cabal](https://github.com/hspec/sensei/blob/master/sensei.cabal)
 * Given this [package.yaml](https://github.com/haskell-compat/base-orphans/blob/master/package.yaml) running `hpack` will generate [base-orphans.cabal](https://github.com/haskell-compat/base-orphans/blob/master/base-orphans.cabal)

## Vim integration

To run `hpack` automatically on modifications to `package.yml` add the
following to your `~/.vimrc`:

```vim
autocmd BufWritePost package.yml silent !hpack
```
