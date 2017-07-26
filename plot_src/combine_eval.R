#!/usr/bin/env Rscript

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
                "PREFIX",
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

eval_prefix = args[1]
lockBinding("eval_prefix", globalenv())

path_files = sprintf("eval_data/%s", eval_prefix)
stopifnot(dir.exists(path_files))

case_data = list.files(path = path_files, pattern = "^case_\\d+\\.RData$")
stopifnot(length(case_data) > 1L)

library(abind)

combine_vars = list(
    "objective_general_avg",
    "objective_general_dev",
    "objective_u_avg",
    "objective_u_dev",
    "objective_x_avg",
    "objective_x_dev",
    "proc_t_general_avg",
    "proc_t_general_dev",
    "objective_general_history",
    "objective_u_history",
    "objective_x_history",
    "proc_t_general_history"
)

# combination
for(j in 1L:length(case_data)) {
    load(sprintf("%s/%s", path_files, case_data[j]))


    if(j > 1L) {
        for(combine_var in combine_vars) {
            combined_var = sprintf("%s_combined", combine_var)
            do.call(what = "<<-", args = list(
                combined_var, abind(
                    eval(parse(text = combined_var)),
                    eval(parse(text = combine_var)),
                    along = 1
                )
            ))
        }
    } else {
        for(combine_var in combine_vars) {
            combined_var = sprintf("%s_combined", combine_var)
            do.call(what = "<<-", args = list(
                combined_var,
                eval(parse(text = combine_var))
            ))
        }
    }
}

# copy combined variables back to original names
for(combine_var in combine_vars) {
    combined_var = sprintf("%s_combined", combine_var)
    do.call(what = "<<-", args = list(
        combine_var,
        eval(parse(text = combined_var))
    ))
    rm(list = combined_var)
}
unlockBinding("eval_prefix", globalenv())
rm(list = c(
    "combine_vars",
    "combine_var",
    "combined_var",
    "eval_prefix",
    "j"
))
save.image(file = sprintf("%s.RData", path_files))
