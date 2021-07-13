#!/bin/bash

input=/usr/local/cs/jdk-16.0.1/lib/modules
# input=hw3.html
printf "\n\n\n"
strace -cf gzip <$input >gzip.gz
printf "\n\n\n"
strace -cf pigz <$input >pigz.gz
printf "\n\n\n"
strace -cf java Pigzj <$input >Pigzj.gz