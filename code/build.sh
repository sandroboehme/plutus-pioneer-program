#!/usr/bin/env sh

cd plutus
git fetch
git checkout $1
nix build -f default.nix plutus.haskell.packages.plutus-core.components.library
nix-build -A plutus-playground.client
nix-build -A plutus-playground.server
nix-build -A plutus-playground.generate-purescript
nix-build -A plutus-playground.start-backend
nix-build -A plutus-pab
# build the docs
nix-build -A plutus-playground.haddock

cd result/share/doc
open .

say build finished

nix-shell
# Continue here: https://docs.plutus-community.com/docs/setup/MacOS.html
