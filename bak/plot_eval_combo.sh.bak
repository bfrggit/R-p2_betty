#!/bin/bash -x

declare -a algs=("greedy_1" "greedy_2" "grd_2_ut" "grd_2_cv")
declare -a mobx=("a" "b" "c")
declare -a tezt=("speed" "n")
declare -a cons=("full" "quota")

# fill_1 unique cases, no constraint specifier (full or quota)
for tezj in "${tezt[@]}"; do
	plot_src/plot_eval_${tezj}.R fill_1_mob_300_4_${tezj}
done

for mobj in "${mobx[@]}"; do
	plot_src/plot_eval_mob_x.R fill_1_mob_300_4_mob_${mobj}
done

plot_src/plot_eval_quota.R fill_1_mob_300_4_quota

# other cases
for algj in "${algs[@]}"; do
	for conj in "${cons[@]}"; do
		for tezj in "${tezt[@]}"; do
			plot_src/plot_eval_${tezj}.R ${algj}_mob_300_4_${tezj}_${conj}
		done

		for mobj in "${mobx[@]}"; do
			plot_src/plot_eval_mob_x.R ${algj}_mob_300_4_mob_${mobj}_${conj}
		done
	done

	plot_src/plot_eval_quota.R ${algj}_mob_300_4_quota_quota
done
