#!/bin/bash -x

plot_res_src/plot_res_metric_eval_mob_x.R a overall 10 0 2
plot_res_src/plot_res_metric_eval_mob_x.R a cover 10 0 2
plot_res_src/plot_res_metric_eval_mob_x.R a util 10 0 1
plot_res_src/plot_res_metric_eval_mob_x.R a traffic 10 0 1

plot_res_src/plot_res_metric_eval_mob_x.R b overall 4 2 2
plot_res_src/plot_res_metric_eval_mob_x.R b cover 4 2 2
plot_res_src/plot_res_metric_eval_mob_x.R b util 4 2 2.2
plot_res_src/plot_res_metric_eval_mob_x.R b traffic 4 2 2.2

plot_res_src/plot_res_metric_eval_mob_x.R c overall 4 0 3
plot_res_src/plot_res_metric_eval_mob_x.R c cover 4 0 0
plot_res_src/plot_res_metric_eval_mob_x.R c util 4 0 2.8
plot_res_src/plot_res_metric_eval_mob_x.R c traffic 4 0 2.8
