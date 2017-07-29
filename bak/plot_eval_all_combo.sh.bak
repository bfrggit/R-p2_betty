#!/bin/bash -x

declare -a mobx=("a" "b" "c")
declare -a tezt=("speed" "quota" "n")
declare -a plod=("objective" "overall" "proc_t")

for ploj in "${plod[@]}"; do
	for tezj in "${tezt[@]}"; do
		Rscript plot_src/plot_all_${ploj}_eval_${tezj}.R
	done

	for mobj in "${mobx[@]}"; do
		Rscript plot_src/plot_all_${ploj}_eval_mob_x.R ${mobj}
	done
done
