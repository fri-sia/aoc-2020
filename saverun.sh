#!/bin/bash

echo $1.fs

while inotifywait -e close_write $1.fs; do 
        echo "test"
        #fsharpc $1.fs
        #touch output
        #mono $1.exe
done
