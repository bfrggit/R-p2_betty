cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

args        = commandArgs(trailingOnly = TRUE)
args_full   = commandArgs(trailingOnly = FALSE)
argc        = length(args)
argc_full   = length(args_full)

if(argc != 1L) {
    if(argc_full > 0L) {
        write(
            paste(c(
                "Usage:",
                args_full[1L:(argc_full - argc)],
                "X_CASE",
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

x_case = as.integer(args[1])
stopifnot(x_case == as.numeric(args[1]))

suppressPackageStartupMessages(require(methods))

gear_rate_all = seq(0.1, 2, by = 0.1) # length = 20
stopifnot(length(gear_rate_all) >= x_case)

num_nodes = 50L
gear_rate = gear_rate_all[x_case]
data_quota = 3500000L
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/lyap_grd.R")

out_path = "eval_data/lyap_grd_mob_300_4_speed_quota"
if(!dir.exists(out_path))
    dir.create(out_path, recursive = TRUE, mode = "0775")

calc_work_mat_f = get_calc_work_mat_lyap_grd_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    gamma_l = 1e-7
)
save_to_file = sprintf("%s/case_%d.RData", out_path, x_case)

source("common/eval_speed_src.R")
