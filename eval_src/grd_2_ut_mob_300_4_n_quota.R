cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = c(seq(5L, 25L, by = 5L), seq(30L, 200L, by = 10L))
data_quota = 3500000L
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/grd_2_ut.R")

calc_work_mat_f = get_calc_work_mat_grd_2_ut_f(
    gamma_u = gamma_u,
    gamma_y = gamma_y * 1 * gamma_u / (gamma_x + gamma_u)
)
save_to_file = "eval_data/grd_2_ut_mob_300_4_n_quota.RData"

source("common/eval_n_src.R")
