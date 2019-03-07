#!/bin/bash

touch concat.txt

for filename in ./*.md; do
    cat $filename >> concat.txt
    echo "" >> concat.txt
done
