#!/bin/sh
mkdir -p _shake
ghc -iplotting --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"
