cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = 50L
num_mob = c(
    seq(0L, 18L, by = 2L),
    seq(20L, 50L, by = 5L)
)
data_quota = 3500000L
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/greedy_2.R")

calc_work_mat_f = get_calc_work_mat_greedy_2_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y
)
save_to_file = "eval_data/greedy_2_mob_300_4_mob_b_quota.RData"

source("common/eval_mob_b_src.R")
