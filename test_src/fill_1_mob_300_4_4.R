cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

source("common/settings.R")
load("prep_RData/impact_multi_capacity_sm_4.RData")
load("prep_RData/mob_300_4_4.RData")

source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1
save_to_file = NA

source("common/test_src.R")
