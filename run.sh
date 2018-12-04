#!/bin/bash
EXT=`dirname "$1"`
FILENAME=`basename -- "$1"`
FILENAME="${FILENAME%.*}"
rm $EXT/$FILENAME

./compile.sh "$1" $FILENAME $EXT
./bin/$FILENAME ./data/$FILENAME.txt
