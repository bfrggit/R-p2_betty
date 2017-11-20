cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

suppressPackageStartupMessages(require(methods))

num_nodes = 50L
num_mob = seq(0L, 50L, by = 2L)
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/lyap_grd.R")

calc_work_mat_f = get_calc_work_mat_lyap_grd_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    gamma_l = 5e-9
)
save_to_file = "eval_data/lyap_grd_mob_300_4_mob_b_full.RData"

source("common/eval_mob_b_src.R")
