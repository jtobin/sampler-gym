#!/bin/bash
ghc GaussMix_Flat.hs -O2 -fllvm -rtsopts -threaded -eventlog 
rm *.hi *.o
strip GaussMix_Flat
mv GaussMix_Flat ~/projects/sampler-gym/demos

