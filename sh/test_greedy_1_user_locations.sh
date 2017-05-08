#!/bin/bash

di=`pwd`
ts=`date +%s`
cd ~/proj/R-p2/
Rscript \
	-o "${di}/test_greedy_1_user_locations_${ts}_out.log" \
	-e "${di}/test_greedy_1_user_locations_${ts}_err.log" \
	test_greedy_1_user_locations.R