#!/bin/bash -e

FULLPATH="$1"
FILENAME="$2"
EXT="$3"

mkdir -p ./bin

if [ $EXT == "hs" ]; then
	ghc -dynamic -O2 --make "$FULLPATH" -threaded -rtsopts -o "./bin/$FILENAME"
elif [ $EXT == "cpp" ]; then
	g++ "$FULLPATH" -o "./bin/$FILENAME" -O2
fi
