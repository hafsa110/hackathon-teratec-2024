#!/bin/bash

threadsomp="$1" 
finalfile="./resultats_omp_$1.txt"

export OMP_NUM_THREADS="$threadsomp" 

echo "Exécution avec $threadsomp threads pour OpenMP, resultats dans le fichier $finalfile"

./RiemannSiegel 10 1000 100 >> "$finalfile"
./RiemannSiegel 10 10000 10 >> "$finalfile"
./RiemannSiegel 10 100000 10 >> "$finalfile"
./RiemannSiegel 10 100000 100 >> "$finalfile"
./RiemannSiegel 10 1000000     100 >> "$finalfile"
./RiemannSiegel 10 10000000    100 >> "$finalfile"
./RiemannSiegel 10 100000000   100 >> "$finalfile"
./RiemannSiegel 10 1000000000  100 >> "$finalfile"
./RiemannSiegel 10 10000000000 100 >> "$finalfile"
