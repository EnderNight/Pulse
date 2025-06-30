#!/bin/sh

VERSION='v0.2.0'
NAME="pulse-$VERSION-linux"

dune clean
dune build @install
dune install --prefix="./$NAME"

rm -rvf "$NAME/lib"
mv -v "$NAME/doc/pulse/"* "$NAME/doc"
rmdir -v "$NAME/doc/pulse"
cp -rv docs "$NAME/doc"

tar czvf "$NAME.tar.gz" "$NAME"

rm -rvf "$NAME"
