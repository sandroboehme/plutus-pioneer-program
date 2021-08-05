#!/usr/bin/env sh

# Source: https://docs.plutus-community.com/docs/setup/MacOS.html
cd plutus
git fetch
git checkout $1
nix --experimental-features nix-command build -f default.nix plutus.haskell.packages.plutus-core.components.library
# that doesn't work anymore:
# `nix build -f default.nix plutus.haskell.packages.plutus-core.components.library`
# It returns `error: experimental Nix feature 'nix-command' is disabled; use '--experimental-features nix-command' to override`
nix-build -A plutus-playground.client
nix-build -A plutus-playground.server
nix-build -A plutus-playground.generate-purescript
nix-build -A plutus-playground.start-backend
nix-build -A plutus-pab
# build the docs
nix-build -A plutus-playground.haddock

say build finishd starting nix shell

nix-shell
