rm(list = ls())

load("test_data/batch_user_locations_n.RData")

library(ggplot2)

num_loops = 5L
df = data.frame(objective_general_avg)
df_sd = data.frame(objective_general_dev)
df$nodes = as.integer(rownames(objective_general_avg))
df$overall_se = df_sd$overall * qnorm(0.975) / sqrt(num_loops)
df$cover_se = df_sd$cover * qnorm(0.975) / sqrt(num_loops)
df$util_se = df_sd$util * qnorm(0.975) / sqrt(num_loops)
df$nact_se = df_sd$nact * qnorm(0.975) / sqrt(num_loops)
plot_obj = ggplot(data = df, aes(x = nodes)) +
    xlab("Number of nodes") +
    ylab("Objectives") +
    geom_area(aes(y = traffic / 40000, fill = "traffic"), alpha = 0.3) +
    geom_errorbar(aes(
        ymin = nact - nact_se,
        ymax = nact + nact_se,
        color = "8_nact"
    ), size = 0.5) + geom_line(aes(y = nact, color = "8_nact"), size = 1) +
    geom_point(aes(y = nact, color = "8_nact", shape = "8_nact"), size = 2) +
    geom_errorbar(aes(
        ymin = util - util_se,
        ymax = util + util_se,
        color = "6_util"
    ), size = 0.5) + geom_line(aes(y = util, color = "6_util"), size = 1) +
    geom_point(aes(y = util, color = "6_util", shape = "6_util"), size = 2) +
    geom_errorbar(aes(
        ymin = cover - cover_se,
        ymax = cover + cover_se,
        color = "4_cover"
    ), size = 0.5) + geom_line(aes(y = cover, color = "4_cover"), size = 1) +
    geom_point(aes(y = cover, color = "4_cover", shape = "4_cover"), size = 2) +
    geom_errorbar(aes(
        ymin = overall - overall_se,
        ymax = overall + overall_se,
        color = "2_all"
    ), size = 0.5) + geom_line(aes(y = overall, color = "2_all"), size = 1) +
    geom_point(aes(y = overall, color = "2_all", shape = "2_all"), size = 2) +
    scale_y_continuous(
        sec.axis = sec_axis(
            ~ . * 40000,
            name = "Data generation rate (byte / sec)"
        )
    ) +
    scale_color_manual(
        name = "Objectives",
        labels = c(
            "2_all"     = "Overall",
            "4_cover"   = "Coverage",
            "6_util"    = "Utility",
            "8_nact"    = "Active nodes"
        ), values = c(
            "2_all"     = "#333333",
            "4_cover"   = "#003399",
            "6_util"    = "#330099",
            "8_nact"    = "#993300"
        )
    ) + scale_fill_manual(
        name = NULL,
        labels = c(
            "traffic"   = "Data generation"
        ), values = c("#cc3333")
    ) + scale_shape_manual(
        name = "Objectives",
        labels = c(
            "2_all"     = "Overall",
            "4_cover"   = "Coverage",
            "6_util"    = "Utility",
            "8_nact"    = "Active nodes"
        ), values = c(
            "2_all"     = 16,
            "4_cover"   = 15,
            "6_util"    = 17,
            "8_nact"    = 18
        )
    )
ggsave(
    filename = "test_plot/batch_user_locations_n.png",
    plot = plot_obj,
    device = "png",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
ggsave(
    filename = "test_plot/batch_user_locations_n.pdf",
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
