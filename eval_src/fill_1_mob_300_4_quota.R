cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

data_quota = as.integer(2 ** seq(16, 23, by = 1 / 3))
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1
save_to_file = "eval_data/fill_1_mob_300_4_quota.RData"

source("common/eval_quota_src.R")
