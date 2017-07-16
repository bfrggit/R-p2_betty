#!/bin/bash -x

plot_res_src/plot_res_metric_eval_mob_x.R a overall 10 0 2
plot_res_src/plot_res_metric_eval_mob_x.R a cover 10 0 2
plot_res_src/plot_res_metric_eval_mob_x.R a util 10 0 1
plot_res_src/plot_res_metric_eval_mob_x.R a traffic 10 0 1
plot_res_src/plot_res_metric_eval_mob_x.R a nact 10 0 1

plot_res_src/plot_res_metric_eval_mob_x.R b overall 4 2 2
plot_res_src/plot_res_metric_eval_mob_x.R b cover 4 2 2
plot_res_src/plot_res_metric_eval_mob_x.R b util 4 2 2.2
plot_res_src/plot_res_metric_eval_mob_x.R b traffic 4 2 2.2
plot_res_src/plot_res_metric_eval_mob_x.R b nact 4 2 2.2

plot_res_src/plot_res_metric_eval_mob_x.R c overall 4 0 3
plot_res_src/plot_res_metric_eval_mob_x.R c cover 4 0 0
plot_res_src/plot_res_metric_eval_mob_x.R c util 4 0 2.8
plot_res_src/plot_res_metric_eval_mob_x.R c traffic 4 0 2.2
plot_res_src/plot_res_metric_eval_mob_x.R c nact 4 0 2.2

plot_res_src/plot_res_metric_eval_n.R overall 10 0 2
plot_res_src/plot_res_metric_eval_n.R cover 10 0 2
plot_res_src/plot_res_metric_eval_n.R util 10 0 1
plot_res_src/plot_res_metric_eval_n.R traffic 10 0 1
plot_res_src/plot_res_metric_eval_n.R nact 10 0 1

plot_res_src/plot_res_metric_eval_speed.R overall 1 0 2
plot_res_src/plot_res_metric_eval_speed.R cover 1 0 2
plot_res_src/plot_res_metric_eval_speed.R util 1 0 2.2
plot_res_src/plot_res_metric_eval_speed.R traffic 1 0 2.2
plot_res_src/plot_res_metric_eval_speed.R nact 1 0 2.2
