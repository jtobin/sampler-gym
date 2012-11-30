#!/bin/bash
# ./program \
#    programArgs \ 
#    rtsArgs \
#    >  stdout goes here \
#    2> stderr goes here 

dpObservations=100
dpAlpha=1.0
gridUpperBound=100
nFlatSamples=1000
nFlatParticles=1000
burnIn=0
thinEvery=250
prngSeed=42
genType="DirichletProcess"

NOW=$(date +"%Y-%m-%d-%H%M")

./GaussMix_flat \
    $dpObservations $dpAlpha $gridUpperBound $nFlatSamples $nFlatParticles $burnIn $thinEvery $prngSeed $genType \
    +RTS -N -qg -s \
    >  ~/projects/sampler-gym/data/output/GaussianMixture/"$genType"/gaussMix_"$gridUpperBound"_"$nFlatSamples"_"$nFlatParticles"_"$burnIn"_"$thinEvery"_"$prngSeed"_"$NOW".dat \
    2> ~/projects/sampler-gym/data/output/GaussianMixture/"$genType"/gaussMix_"$gridUpperBound"_"$nFlatSamples"_"$nFlatParticles"_"$burnIn"_"$thinEvery"_"$prngSeed"_"$NOW".rts 

