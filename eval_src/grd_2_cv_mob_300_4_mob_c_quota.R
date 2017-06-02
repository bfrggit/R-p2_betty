cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = seq(50L, 200L, by = 6L)
data_quota = 3500000L
capacity_data_format = "prep_RData/impact_multi_cap_dist_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/grd_2_cv.R")

calc_work_mat_f = get_calc_work_mat_grd_2_cv_f(
    gamma_x = gamma_x,
    gamma_y = gamma_y * 1e-3
)
save_to_file = "eval_data/grd_2_cv_mob_300_4_mob_c_quota.RData"

source("common/eval_mob_c_src.R")
