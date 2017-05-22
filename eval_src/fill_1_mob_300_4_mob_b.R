cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = 50L
num_mob = c(
    seq(0L, 18L, by = 2L),
    seq(20L, 50L, by = 5L)
)
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1
save_to_file = "eval_data/fill_1_mob_300_4_mob_b.RData"

source("common/eval_mob_b_src.R")
