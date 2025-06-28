#!/bin/sh

VERSION='v0.1.0'

dune build @install
dune install --prefix="./pulse-$VERSION"

rm -rvf "pulse-$VERSION/lib"
mv -v "pulse-$VERSION/doc/pulse/"* "pulse-$VERSION/doc"
rmdir -v "pulse-$VERSION/doc/pulse"
cp -rv docs "pulse-$VERSION/doc"

tar czvf "pulse-$VERSION.tar.gz" "pulse-$VERSION"

rm -rvf "pulse-$VERSION"
