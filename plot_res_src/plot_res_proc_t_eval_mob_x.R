#!/usr/bin/env Rscript

rm(list = ls())

args        = commandArgs(trailingOnly = TRUE)
args_full   = commandArgs(trailingOnly = FALSE)
argc        = length(args)
argc_full   = length(args_full)

if(argc != 4L) {
    if(argc_full > 0L) {
        write(
            paste(c(
                "Usage:",
                args_full[1L:(argc_full - argc)],
                "SUFFIX",
                "SPARSE", "MODULO", 'LEGEND',
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

mob_suffix = args[1]
lockBinding("mob_suffix", globalenv())

# these two integers define sparsely populated points and error-bar
par_sparse = as.integer(args[2])
par_modulo = as.integer(args[3])
par_legend = as.numeric(args[4])
stopifnot(par_sparse == as.numeric(args[2]))
stopifnot(par_modulo == as.numeric(args[3]))
stopifnot(par_sparse > 0L)
stopifnot(par_modulo >= 0L && par_modulo <= par_sparse)
stopifnot(par_legend >= 0L && par_legend <= 3L)
lockBinding("par_sparse", globalenv())
lockBinding("par_modulo", globalenv())

source("common/plot_res_mappings.R")

library(ggplot2)
library(reshape2)

# constants
exp_set = sprintf("mob_300_4_mob_%s", mob_suffix)
num_loops = 5L
frame = 60
pos_legend = c(par_legend %/% 2L, par_legend - (par_legend %/% 2L) * 2)

# load all data files
data_files = c(
    "fill_1_%s",
    "random_1_%s_quota",
    "ga_1_%s_quota",
    "greedy_1_%s_quota",
    "greedy_2_%s_quota",
    "lyap_grd_%s_quota"
)
data_f_len = length(data_files)
data_fr_ls = list()

for(j in 1L:data_f_len){
    fn = sprintf(data_files[j], exp_set)
    cat(sprintf("Processing data file eval = \"%s\"", fn), "\n")
    load(sprintf("eval_data/%s.RData", fn))

    line_cols = c("nodes", "cal")
    df = data.frame(proc_t_general_avg)
    df_se = data.frame(proc_t_general_dev) * qnorm(0.975) / sqrt(num_loops)
    df$nodes = df_se$nodes = as.integer(rownames(proc_t_general_avg))
    dm = melt(df[, line_cols], id.vars = "nodes", variable.name = "obj")
    dm_se = melt(df_se[, line_cols], id.vars = "nodes", variable.name = "obj")
    dm$se = dm_se$value
    dm$ts = data_files[j]
    data_fr_ls[[j]] = dm
}
dm = do.call("rbind", data_fr_ls)
dm_subset = subset(dm, nodes %% par_sparse == par_modulo)

plot_obj = ggplot(data = dm, aes(x = nodes)) +
    xlab("Number of mobile nodes") +
    ylab("Planning time (sec)") +
    expand_limits(y = 0) +
    geom_line(
        aes(y = value, color = ts, alpha = ts, linetype = ts), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = ts
        ), width = (max(df$nodes) - min(df$nodes)) * 0.03,
        size = 1, alpha = 0.5, data = dm_subset
    ) + geom_point(
        aes(y = value, color = ts, shape = ts, alpha = ts),
        size = 5, stroke = 1.2, data = dm_subset
    ) + scale_color_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_color
    ) + scale_shape_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_shape
    ) + scale_alpha_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_alpha
    ) + scale_linetype_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_linetype
    ) + res_themes + theme(legend.justification = pos_legend
    ) + theme(legend.position = pos_legend
    ) + geom_hline(
        yintercept = frame, color = "orangered"
    ) + annotate(
        "text", label = "Frame", color = "orangered", size = 5,
        x = max(df$nodes),
        y = frame + (
                max(frame, max(dm[dm$obj == "cal", ]$value)) -
                min(0, min(dm[dm$obj == "cal", ]$value))
            ) * 1.6 / 60
    )
cat("Rendering...", "\n")

# plot_obj
ggsave(
    filename = sprintf(
        "results/proc_t_mob_%s_%d-%d.pdf",
        mob_suffix,
        par_sparse, par_modulo
    ),
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
)
cat("Done", "\n")
