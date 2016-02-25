#!/bin/sh

set -eux

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_osx() {
  curl -skL https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
}

fetch_stack_linux() {
  curl -sL https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
}

fetch_tinc() {
  curl -sL https://zalora-public.s3.amazonaws.com/tinc > $HOME/.local/bin/tinc
}

mkdir -p $HOME/.local/bin

case "$BUILD" in
  stack)
    if [ `uname` = "Darwin" ]; then
      travis_retry fetch_stack_osx
    else
      travis_retry fetch_stack_linux
    fi;

    travis_retry stack --no-terminal setup;
    ;;
  tinc)
    travis_retry fetch_tinc
    chmod +x $HOME/.local/bin/tinc
    travis_retry cabal update
    sed -i 's/^jobs:/-- jobs:/' $HOME/.cabal/config
    ;;
esac
