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

num_mob_all = seq(0L, 50L, by = 2L) # length = 26
stopifnot(length(num_mob_all) >= x_case)

num_nodes = 50L
num_mob = num_mob_all[x_case]
capacity_data_format = "prep_RData/impact_multi_cap_omni_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/lyap_grd.R")

out_path = "eval_data/lyap_grd_mob_300_4_mob_b_full"
if(!dir.exists(out_path))
    dir.create(out_path, recursive = TRUE, mode = "0775")

calc_work_mat_f = get_calc_work_mat_lyap_grd_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    gamma_l = 5e-9
)
save_to_file = sprintf("%s/case_%d.RData", out_path, x_case)

source("common/eval_mob_b_src.R")
