#!/usr/bin/env bash

set -eo pipefail

find rest-rewrite rest-rewrite-test -name \*.hs | while read -r SRC_FILE ; do
    hlint "$SRC_FILE" --refactor --refactor-options=-i
done
