#!/bin/bash

input=hw3.html

for i in 1; do
	echo "### $i processors"
	printf "\ngzip:"
	time gzip <$input >gzip.gz
	printf "\npigz:"
	time pigz -p $i <$input >pigz.gz
	printf "\nPigzj:"
	time java Pigzj -p $i <$input >Pigzj.gz
	ls -l gzip.gz pigz.gz Pigzj.gz

	# This checks Pigzj's output.
	pigz -d <Pigzj.gz | cmp - $input
done
