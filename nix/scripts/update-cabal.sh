#!/usr/bin/env -S nix develop --accept-flake-config --command bash 
# shellcheck shell=bash

set -euo pipefail

PACKAGES=("rest-rewrite" "rest-rewrite-test")

echo "Cabal is $(which cabal)"

if [[ ! -f ./flake.nix || ! -f ./cabal.project || ! -d ./nix/packages ]]; then
    echo "This script can only be executed from the root of the repository. Exiting."
    exit 1
fi

echo "Regenerating cabal freeze file with haskell packages available in updated flake..."
if [ -f ./cabal.project.freeze ]; then
    echo "Making a backup of existing cabal.project.freeze..."
    mv ./cabal.project.freeze ./cabal.project.freeze.bak
    echo "Made a backup of existing cabal.project.freeze."
fi
# Note: we use temporary directories to avoid drawing on whatever the user 
# has in their /.cabal directory, since this is visible to us from within 
# the devShell.
CABAL_DIR="$(mktemp -d)" cabal freeze --builddir="$(mktemp -d)" > /dev/null || echo "Failed to regenerate cabal freeze file."
if [ ! -f ./cabal.project.freeze ]; then
    echo "Restoring original cabal.project.freeze..."
    mv ./cabal.project.freeze.bak ./cabal.project.freeze
    echo "Restored original cabal.project.freeze."
    exit 1
else
    echo "Removing backup of original cabal.project.freeze..."
    rm ./cabal.project.freeze.bak
    echo "Removed backup of original cabal.project.freeze."
fi
echo "Regenerated cabal freeze file."

echo "Formatting cabal files..."
echo "Currently in directory: $(pwd)"
for package in "${PACKAGES[@]}"; do
    echo "Formatting cabal file for package $package..."
    cabal format "$package/$package.cabal" || (echo "Failed to format cabal file for $package." && exit 1)
    echo "Formatted cabal file for package $package."
done
echo "Formatted cabal files."

echo "Regenerating nix files..."
cd ./nix/packages
for package in "${PACKAGES[@]}"; do
    echo "Regenerating nix file for package $package..."
    cabal2nix "../../$package" > "./$package.nix" || (echo "Failed to regenerate nix file for $package." && exit 1)
    echo "Regenerated nix file for package $package."
done
echo "Regenerated nix files."

echo "Formatting nix files..."
cd ../..
nix --accept-flake-config fmt -- -q .
echo "Formatted nix files."
