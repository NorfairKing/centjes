#!/usr/bin/env bash

watchexec \
  --restart \
  --exts hs,cabal,yaml,typ \
  --workdir centjes-switzerland/test_resources/example \
  -- cabal run centjes-switzerland -- taxes --config-file switzerland.yaml
