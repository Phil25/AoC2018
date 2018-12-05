#!/bin/bash
EXT=`dirname "$1"`
FILENAME=`basename -- "$1"`
FILENAME="${FILENAME%.*}"
rm ./bin/$FILENAME

./compile.sh "$1" $FILENAME $EXT
./bin/$FILENAME ./data/$FILENAME.txt
