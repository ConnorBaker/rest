#!/usr/bin/env bash

set -euo pipefail

if [[ ! -f ./flake.nix ]]; then
    echo "This script can only be executed from the root of the repository. Exiting."
    exit 1
fi

echo "Updating flake inputs..."
nix flake --accept-flake-config update
echo "Updated flake inputs."