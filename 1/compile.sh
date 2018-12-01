#!/bin/bash -e
ghc -dynamic -O2 --make Main.hs -threaded -rtsopts
