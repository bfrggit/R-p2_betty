cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_mob = c(
    seq(0L, 8L, by = 2L),
    seq(10L, 20L, by = 5L),
    seq(30L, 170L, by = 10L)
)
num_static = 30L
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/greedy_2.R")

calc_work_mat_f = get_calc_work_mat_greedy_2_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y
)
save_to_file = "eval_data/greedy_2_mob_300_4_mob_a_full.RData"

source("common/eval_mob_a_src.R")
