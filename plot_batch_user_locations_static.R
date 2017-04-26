rm(list = ls())

load("test_data/batch_user_locations_static.RData")

library(ggplot2)
library(reshape2)

line_cols = c("mob", "nact", "cover", "util", "overall")
list_general = list()
for(snd in 1L:dim(objective_general_avg)[3]) {
    df = data.frame(objective_general_avg[, , snd])
    df$mob = num_nodes - as.integer(rownames(df))
    dm = melt(df[, line_cols], id.vars = "mob", variable.name = "series")
    df_se = data.frame(objective_general_dev[, , snd]) *
        qnorm(0.975) / sqrt(num_loops)
    df_se$mob = num_nodes - as.integer(rownames(df_se))
    dm_se = melt(df_se[, line_cols], id.vars = "mob", variable.name = "series")
    dm$se = dm_se$value
    list_general[[snd]] = dm
}
df_overall = data.frame(objective_general_avg[, "overall", ])
df_overall$mob = num_nodes - as.integer(rownames(df_overall))
dm_overall = melt(df_overall, id.vars = "mob", variable.name = "t_impact")
df_cover = data.frame(objective_general_avg[, "cover", ])
df_cover$mob = num_nodes - as.integer(rownames(df_cover))
dm_cover = melt(df_cover, id.vars = "mob", variable.name = "t_impact")
plot_obj = ggplot(
    data = list_general[[length(list_general)]],
    aes(x = mob)
) +
    xlab("Number of mobile nodes") +
    ylab("Objective value") +
    geom_area(
        data = df,
        aes(y = traffic / 10000, fill = "traffic"),
        alpha = 0.3
    ) + geom_line(
        data = dm_cover,
        aes(y = value, color = "cover", alpha = t_impact),
        size = 1
    ) + geom_point(
        data = dm_cover,
        aes(y = value, color = "cover", shape = "cover", alpha = t_impact),
        size = 2
    ) + geom_line(
        data = dm_overall,
        aes(y = value, color = "overall", alpha = t_impact),
        size = 1
    ) + geom_point(
        data = dm_overall,
        aes(y = value, color = "overall", shape = "overall", alpha = t_impact),
        size = 2
    )+ geom_line(
        aes(y = value, color = series), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = series
        ), size = 0.5, width = 0.5
    ) + geom_point(
        aes(y = value, color = series, shape = series), size = 2
    ) + scale_y_continuous(
        sec.axis = sec_axis(
            ~ . * 10000,
            name = "Data generation rate (byte / sec)"
        )
    ) + scale_color_manual(
        name = "Objectives",
        labels = c(
            "overall"   = "Overall",
            "cover"     = "Coverage",
            "util"      = "Utility",
            "nact"      = "Active nodes"
        ), values = c(
            "overall"   = "gray15",
            "cover"     = "dodgerblue4",
            "util"      = "purple4",
            "nact"      = "orangered3"
        )
    ) + scale_fill_manual(
        name = NULL,
        labels = c(
            "traffic"   = "Data generation"
        ), values = c(
            "traffic"   = "indianred"
        )
    ) + scale_shape_manual(
        name = "Objectives",
        labels = c(
            "overall"   = "Overall",
            "cover"     = "Coverage",
            "util"      = "Utility",
            "nact"      = "Active nodes"
        ), values = c(
            "overall"   = 16,
            "cover"     = 15,
            "util"      = 17,
            "nact"      = 18
        )
    ) + scale_alpha_manual(
        name = "Temporal impact",
        labels = c(
            "X0"        = "Current frame",
            "X5"        = "5 frames",
            "X10"       = "10 frames",
            "X15"       = "15 frames",
            "X20"       = "20 frames"
        ), values = c(
            "X0"        = 0.2,
            "X5"        = 0.4,
            "X10"       = 0.6,
            "X15"       = 0.8,
            "X20"       = 1.0
        )
    )
plot_obj
# ggsave(
#     filename = "test_plot/batch_user_locations_static.png",
#     plot = plot_obj,
#     device = "png",
#     width = 8,
#     height = 5,
#     units = "in",
#     dpi = 300
# )
# ggsave(
#     filename = "test_plot/batch_user_locations_static.pdf",
#     plot = plot_obj,
#     device = "pdf",
#     width = 8,
#     height = 5,
#     units = "in",
#     dpi = 300
# )
