#!/bin/bash

echo $1.fs

while true; do 
        inotifywait -e close_write $1.fs;
        echo ""
        echo "detected file change..."
        fsharpc $1.fs > /dev/null
        echo ""
        mono $1.exe
        echo ""
done
