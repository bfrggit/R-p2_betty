cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = 50L
gear_rate = seq(0.1, 2, by = 0.1)
data_quota = 3500000L
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/random_1.R")

calc_work_mat_f = get_calc_work_mat_random_1_f(
    seed = 9L
)
save_to_file = "eval_data/random_1_mob_300_4_speed_quota.RData"

source("common/eval_speed_src.R")
