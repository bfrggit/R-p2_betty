#!/usr/bin/env Rscript

rm(list = ls())

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
                "SUFFIX",
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

mob_suffix = args[1]
lockBinding("mob_suffix", globalenv())

source("common/plot_mappings.R")

library(ggplot2)
library(reshape2)

# constants
exp_set = sprintf("mob_300_4_mob_%s", mob_suffix)
num_loops = 5L
trf_scale = 2e+7
quota = 3.5e+6

# load all data files
data_files = c(
    "fill_1_%s",
    "greedy_1_%s_quota",
    "greedy_2_%s_quota",
    "greedy_2_%s_full"
)
data_f_len = length(data_files)
data_fr_ls = list()

for(j in 1L:data_f_len){
    fn = sprintf(data_files[j], exp_set)
    cat(sprintf("Processing data file eval = \"%s\"", fn), "\n")
    load(sprintf("eval_data/%s.RData", fn))

    line_cols = c("nodes", "overall", "trf_n")
    df = data.frame(objective_general_avg)
    df$trf_n = df$traffic / trf_scale
    df_se = data.frame(objective_general_dev) * qnorm(0.975) / sqrt(num_loops)
    df_se$trf_n = df_se$traffic / trf_scale
    df$nodes = df_se$nodes = as.integer(rownames(objective_general_avg))
    dm = melt(df[, line_cols], id.vars = "nodes", variable.name = "obj")
    dm_se = melt(df_se[, line_cols], id.vars = "nodes", variable.name = "obj")
    dm$se = dm_se$value
    dm$ts = data_files[j]
    data_fr_ls[[j]] = dm
}
dm = do.call("rbind", data_fr_ls)

plot_obj = ggplot(data = dm, aes(x = nodes)) +
    xlab("Number of mobile nodes") +
    ylab(pa_obj_label["overall"]) +
    expand_limits(y = 0) +
    geom_hline(
        yintercept = quota / trf_scale, color = "gray40"
    ) + geom_line(
        aes(y = value, color = obj, linetype = ts, alpha = ts), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = obj
        ), width = (max(df$nodes) - min(df$nodes)) * 0.02,
        size = 0.5, alpha = 0.5
    ) + geom_point(
        aes(y = value, color = obj, shape = obj, alpha = ts), size = 2
    ) + scale_y_continuous(
        limits = c(0, 1.2),
        sec.axis = sec_axis(
            ~ . * trf_scale,
            name = "Data generation rate (byte / sec)"
        )
    ) + scale_color_manual(
        name = pa_obj_name, labels = pa_obj_label, values = pa_obj_color
    ) + scale_shape_manual(
        name = pa_obj_name, labels = pa_obj_label, values = pa_obj_shape
    ) + scale_linetype_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_linetype
    ) + scale_alpha_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_alpha
    ) + annotate(
        "text", label = "Quota", color = "gray40",
        x = 0,
        y = quota / trf_scale + 1.2 / 40
    )
cat("Rendering...", "\n")

# plot_obj
ggsave(
    filename = sprintf("eval_plot/all_overall_mob_%s.pdf", mob_suffix),
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
cat("Done", "\n")
