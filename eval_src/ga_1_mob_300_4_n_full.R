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

num_nodes_all = c(seq(5L, 25L, by = 5L), seq(30L, 200L, by = 10L)) # length = 23
stopifnot(length(num_nodes_all) >= x_case)

num_nodes = num_nodes_all[x_case]
capacity_data_format = "prep_RData/impact_multi_capacity_sm_%d.RData"
mobility_data_format = "prep_RData/mob_300_4_%d.RData"

source("common/settings.R")
source("solution/ga_1.R")

out_path = "eval_data/ga_1_mob_300_4_n_full"
if(!dir.exists(out_path))
    dir.create(out_path, recursive = TRUE, mode = "0775")

calc_work_mat_f = get_calc_work_mat_ga_1_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    parallel = 2L,
    seed = 9L
)
save_to_file = sprintf("%s/case_%d.RData", out_path, x_case)

source("common/eval_n_src.R")
