#!/usr/bin/env bash

export DEVELOPMENT=True
export CENTJES_DOCS_SITE_LOG_LEVEL=Debug

watchexec \
  --restart \
  --exts hs,cabal,yaml,hamlet,julius,lucius,cassius \
  --workdir centjes-docs-site \
  -- cabal run centjes-docs-site -- "$@"
