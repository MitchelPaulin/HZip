#!/bin/bash

if [ "$#" = 1 ] && [ "$1" = "-p" ]; then
    ghc -O2 -XBinaryLiterals lz77.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
else
    ghc -O2 -XBinaryLiterals lz77.hs -fforce-recomp
    ghc --make -O2 -XBinaryLiterals lzss.hs lz77.hs -fforce-recomp
fi