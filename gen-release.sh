#!/bin/sh

VERSION='v0.1'

dune build @install --verbose
dune install --prefix="./pulse-$VERSION" --verbose
rm -rvf "pulse-$VERSION/lib"
tar czvf "pulse-$VERSION.tar.gz" "pulse-$VERSION"
rm -rvf "pulse-$VERSION"
