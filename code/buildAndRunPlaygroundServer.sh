#!/usr/bin/env sh

# source: https://docs.plutus-community.com/docs/setup/MacOS.html

# in the plutus folder
# after $>nix-shell

cd plutus-pab
plutus-pab-generate-purs
cd ../plutus-playground-server
plutus-playground-generate-purs
plutus-playground-server

