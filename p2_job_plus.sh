#!/usr/bin/env bash

if [ "$#" -lt 2 ]; then
	echo >&2 "Usage: $(basename $0) DIRECTORY SCRIPT [...]"
	exit -1
fi

if [ ! -d $1 ]; then
	echo >&2 "Error: $1 is not a directory"
	exit -2
fi
cd $1
if [ ! -f $2 ]; then
	echo >&2 "Error: $2 does not exist"
	exit -3
fi

if type module >/dev/null 2>&1; then
	module load R
fi
Rscript $2 ${@:3}
