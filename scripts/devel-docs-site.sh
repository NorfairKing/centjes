#!/usr/bin/env bash

export DEVELOPMENT=True

stack install centjes-docs-site \
  --file-watch --watch-all \
  --fast \
  --ghc-options="-freverse-errors -j4 +RTS -A128M -n2m -RTS" \
  --exec="./scripts/restart-docs-site.sh $@"

