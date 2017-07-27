cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

data_quota = 3500000L

source("common/settings.R")
load("prep_RData/impact_multi_capacity_sm_4.RData")
load("prep_RData/mob_300_4_4.RData")

source("solution/ga_1.R")

calc_work_mat_f = get_calc_work_mat_ga_1_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    parallel = 2L,
    seed = 9L
)
save_to_file = "test_data/ga_1_par_mob_300_4_4_quota.RData"

source("common/test_src.R")
