#!/bin/bash

if [ "$#" = 1 ] && [ "$1" = "-p" ]; then
    ghc -O2 lz77.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
else
    ghc -O2 lz77.hs -fforce-recomp
fi