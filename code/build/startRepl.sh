#!/usr/bin/env sh

# Run in in the 'nix-shell' in the plutus folder
path='plutus-pioneer-program/code/week'$1
cd "${path}"
cabal update
cabal build
say Build finished. Opening repl.
cabal repl
