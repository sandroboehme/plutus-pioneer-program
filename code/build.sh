#!/usr/bin/env sh

# in the plutus folder
git fetch
git checkout 2fbb7abb22138a434bb6c4f663a81e9b9dc51e98
nix build -f default.nix plutus.haskell.packages.plutus-core.components.library
nix-build -A plutus-playground.client
nix-build -A plutus-playground.server
nix-build -A plutus-playground.generate-purescript
nix-build -A plutus-playground.start-backend
nix-build -A plutus-pab

# Continue here: https://docs.plutus-community.com/docs/setup/MacOS.html
