#!/usr/bin/env sh

# source: https://docs.plutus-community.com/docs/setup/MacOS.html

# after executing `.build.sh` which should end up in the `nix-shell`

cd plutus/plutus-pab
plutus-pab-generate-purs
cd ../plutus-playground-server
plutus-playground-generate-purs

say starting playground server

plutus-playground-server

