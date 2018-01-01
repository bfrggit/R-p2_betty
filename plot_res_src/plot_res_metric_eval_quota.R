#!/usr/bin/env Rscript

rm(list = ls())

args        = commandArgs(trailingOnly = TRUE)
args_full   = commandArgs(trailingOnly = FALSE)
argc        = length(args)
argc_full   = length(args_full)

if(argc != 2L) {
    if(argc_full > 0L) {
        write(
            paste(c(
                "Usage:",
                args_full[1L:(argc_full - argc)],
                "METRIC",
                'LEGEND',
                "\n"
            ), collapse = " "),
            stderr()
        )
    }
    stop("Invalid argument(s)")
}

nom_metric = args[1]
lockBinding("nom_metric", globalenv())

par_legend = as.numeric(args[2])
stopifnot(par_legend >= 0L && par_legend <= 3L)

source("common/plot_res_mappings.R")

library(ggplot2)
library(reshape2)
library(Cairo)

# name of metric must be valid, as listed in imported mappings
stopifnot(nom_metric %in% names(pa_obj_label))

# constants
exp_set = sprintf("mob_300_4_quota")
num_loops = 5L
pos_legend = c(par_legend %/% 2L, par_legend - (par_legend %/% 2L) * 2)

# load all data files
data_files = c(
    "fill_1_%s",
    "random_1_%s_quota",
    # "ga_1_%s_quota",
    # "greedy_1_%s_quota",
    "greedy_2_%s_quota",
    "lyap_grd_%s_quota"
)
data_f_len = length(data_files)
data_fr_ls = list()

for(j in 1L:data_f_len){
    fn = sprintf(data_files[j], exp_set)
    cat(sprintf("Processing data file eval = \"%s\"", fn), "\n")
    load(sprintf("eval_data/%s.RData", fn))

    line_cols = c("quota", nom_metric)
    df = data.frame(objective_general_avg)
    df_se = data.frame(objective_general_dev) * qnorm(0.975) / sqrt(num_loops)
    df$quota = df_se$quota = as.numeric(rownames(objective_general_avg))
    dm = melt(df[, line_cols], id.vars = "quota", variable.name = "obj")
    dm_se = melt(df_se[, line_cols], id.vars = "quota", variable.name = "obj")
    dm$se = dm_se$value
    dm$ts = data_files[j]
    data_fr_ls[[j]] = dm
}
dm = do.call("rbind", data_fr_ls)

plot_obj = ggplot(data = dm, aes(x = quota)) + scale_x_log10() +
    xlab("Data quota (byte / sec)") +
    ylab(pa_obj_label[nom_metric]) +
    # expand_limits(y = 0) +
    geom_line(
        aes(y = value, color = ts, alpha = ts, linetype = ts), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = ts
        ), width = log10(max(df$quota) / min(df$quota)) * 0.03,
        size = 1, alpha = 0.5
    ) + geom_point(
        aes(y = value, color = ts, shape = ts, alpha = ts),
        size = 5, stroke = 1.2
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
    )
if(nom_metric %in% c("nact", "util", "traffic")) {
    plot_obj = plot_obj + expand_limits(y = 0)
}
if(nom_metric == "traffic") {
    plot_obj = plot_obj + geom_line(
        aes(y = quota), color = "orangered", data = df
    ) + annotate(
        "text", label = "Quota", color = "orangered", size = 5,
        x = max(df$quota),
        y = max(df$quota) + (
                max(dm[dm$obj == "traffic", ]$value) -
                min(0, min(dm[dm$obj == "traffic", ]$value))
            ) * 5e-2
    )
}
cat("Rendering...", "\n")

# plot_obj
ggsave(
    filename = sprintf(
        "results/%s_quota.pdf",
        nom_metric
    ),
    plot = plot_obj,
    device = cairo_pdf,
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
)
cat("Done", "\n")
