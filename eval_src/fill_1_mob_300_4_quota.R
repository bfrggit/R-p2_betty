cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

data_quota = c(
    seq(      0L,   500000L, by = 100000L),
    seq( 600000L,  1000000L, by = 200000L),
    seq(2000000L,  5000000L, by = 500000L),
    seq(6000000L, 10000000L, by = 1000000L)
)
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1
save_to_file = "eval_data/fill_1_mob_300_4_quota.RData"

source("common/eval_quota_src.R")
