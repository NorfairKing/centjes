#!/usr/bin/env bash


cd centjes-docs-site

killall centjes-docs-site || true

sleep 0.25

export CENTJES_DOCS_SITE_LOG_LEVEL=Debug

centjes-docs-site &
