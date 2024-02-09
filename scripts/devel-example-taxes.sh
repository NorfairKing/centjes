#!/usr/bin/env bash

cd centjes-switzerland

stack install centjes-switzerland \
  --file-watch --watch-all \
  --fast \
  --ghc-options="-freverse-errors -j4 +RTS -A128M -n2m -RTS" \
  --exec="centjes-switzerland taxes --config-file test_resources/example/switzerland.yaml"

