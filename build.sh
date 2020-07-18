#!/bin/bash

if [ "$#" = 1 ] && [ "$1" = "-p" ]; then
    ghc -O2 -XBinaryLiterals src/lz77.hs src/lz77Common.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
else
    ghc --make -O2 -Wall -XBinaryLiterals src/lz77.hs src/lz77Common.hs -fforce-recomp
    ghc --make -O2 -Wall -XBinaryLiterals src/lzss.hs src/lz77Common.hs -fforce-recomp
fi