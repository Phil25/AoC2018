#!/bin/bash -e
ghc -dynamic -O2 --make "$1/$2" -threaded -rtsopts -o "$1/Main"
