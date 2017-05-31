#!/usr/bin/env Rscript

rm(list = ls())

source("common/plot_mappings.R")

library(ggplot2)
library(reshape2)

# constants
exp_set = "mob_300_4_speed"
num_loops = 5L

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

    line_cols = c("speed", "nact", "cover", "util")
    df = data.frame(objective_general_avg)
    df_se = data.frame(objective_general_dev) * qnorm(0.975) / sqrt(num_loops)
    df$speed = df_se$speed = as.numeric(rownames(objective_general_avg))
    dm = melt(df[, line_cols], id.vars = "speed", variable.name = "obj")
    dm_se = melt(df_se[, line_cols], id.vars = "speed", variable.name = "obj")
    dm$se = dm_se$value
    dm$ts = data_files[j]
    data_fr_ls[[j]] = dm
}
dm = do.call("rbind", data_fr_ls)

plot_obj = ggplot(data = dm, aes(x = speed)) +
    xlab("Speed multiplier") +
    ylab("Objectives") +
    expand_limits(y = 0) +
    geom_line(
        aes(y = value, color = obj, linetype = ts, alpha = ts), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = obj
        ), width = (max(df$speed) - min(df$speed)) * 0.02,
        size = 0.5, alpha = 0.5
    ) + geom_point(
        aes(y = value, color = obj, shape = obj, alpha = ts), size = 2
    ) + scale_y_continuous(
        limits = c(0, 1)
    ) + scale_color_manual(
        name = pa_obj_name, labels = pa_obj_label, values = pa_obj_color
    ) + scale_shape_manual(
        name = pa_obj_name, labels = pa_obj_label, values = pa_obj_shape
    ) + scale_linetype_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_linetype
    ) + scale_alpha_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_alpha
    )
cat("Rendering...", "\n")

# plot_obj
ggsave(
    filename = sprintf("eval_plot/all_objective_speed.pdf"),
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
cat("Done", "\n")
