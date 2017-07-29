cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

data_quota = 3500000L

source("common/settings.R")
load("prep_RData/impact_multi_capacity_sm_4.RData")
load("prep_RData/mob_300_4_4.RData")

source("solution/random_1.R")

calc_work_mat_f = get_calc_work_mat_random_1_f(
    seed = 9L
)
save_to_file = "test_data/random_1_mob_300_4_4_quota.RData"

source("common/test_src.R")
