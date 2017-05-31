#!/usr/bin/env Rscript

rm(list = ls())

source("common/plot_mappings.R")

library(ggplot2)
library(reshape2)

# constants
exp_set = "mob_300_4_n"
num_loops = 5L
frame = 60

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

plot_obj = ggplot(data = dm, aes(x = nodes)) +
    xlab("Number of nodes") +
    ylab("Planning time (sec)") +
    expand_limits(y = 0) +
    geom_hline(
        yintercept = frame, color = "gray40"
    ) + geom_line(
        aes(y = value, alpha = ts, color = -value), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se
        ), width = (max(df$nodes) - min(df$nodes)) * 0.02,
        size = 0.5, color = "gray20", alpha = 0.5
    ) + geom_point(
        aes(y = value, shape = ts, alpha = ts), size = 2, color = "gray20"
    ) + scale_y_continuous(
        limits = c(0, 64)
    ) + scale_shape_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pp_sol_shape
    ) + scale_alpha_manual(
        name = pa_sol_name, labels = pa_sol_label, values = pa_sol_alpha
    ) + scale_color_gradientn(
        colors = rainbow(4, start = 0, end = 0.6), guide = FALSE
    ) + annotate(
        "text", label = "Frame", color = "gray40",
        x = 0,
        y = frame - 64 / 40
    )
cat("Rendering...", "\n")

# plot_obj
ggsave(
    filename = sprintf("eval_plot/all_proc_t_n.pdf"),
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
cat("Done", "\n")
