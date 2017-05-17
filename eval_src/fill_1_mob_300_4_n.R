cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = c(seq(5L, 25L, by = 5L), seq(30L, 200L, by = 10L))
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1
save_to_file = "eval_data/fill_1_mob_300_4_n.RData"

source("common/eval_n_src.R")
