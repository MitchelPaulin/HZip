#!/bin/bash

if [ "$#" = 1 ] && [ "$1" = "-p" ]; then
    ghc -O2 src/lz77.hs src/lz77Common.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
    ghc -O2 src/lzss.hs src/lz77Common.hs src/bits.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
else
    ghc --make -O2 -Wall src/lz77.hs src/lz77Common.hs -fforce-recomp
    ghc --make -O2 -Wall src/lzss.hs src/lz77Common.hs src/bits.hs -fforce-recomp
fi