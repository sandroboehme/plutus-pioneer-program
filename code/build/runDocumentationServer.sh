#!/usr/bin/env sh

# Source: https://github.com/nstankov-bg/docs.plutus-community.com/pull/61

# after executing `.build.sh` but the nix shell is not needed

cd plutus/result/share/doc
python3 -m http.server 8088
