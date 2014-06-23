#! /bin/sh
set -e
set -x
alex Scanner.x
happy FParser.y
ghc --make -O -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances Main -o ../bin/pfc
