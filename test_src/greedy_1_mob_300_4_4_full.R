cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

source("common/settings.R")
load("prep_RData/impact_multi_capacity_sm_4.RData")
load("prep_RData/mob_300_4_4.RData")

source("solution/greedy_1.R")

calc_work_mat_f = get_calc_work_mat_greedy_1_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y
)
save_to_file = "test_data/greedy_1_mob_300_4_4_full.RData"

source("common/test_src.R")
