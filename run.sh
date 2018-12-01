#!/bin/bash
rm "$1"/Main

../compile.sh "$1" Main.hs
"$1"/Main data.txt
