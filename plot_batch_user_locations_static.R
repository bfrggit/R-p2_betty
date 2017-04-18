rm(list = ls())

load("test_data/batch_user_locations_static.RData")

library(ggplot2)

df_list = list()
for(snd in 1L:dim(objective_avg_avg)[3]) {
    df_list[[snd]] = data.frame(objective_avg_avg[, , snd])
}
plot_obj = ggplot(
    data = df_list[[length(df_list)]],
    aes(x = df_list[[1]][1, 1] - static)
) +
    xlab("Number of mobile nodes") +
    ylab("Objective value") +
    geom_area(aes(y = traffic / 10000, fill = "Data generation"), alpha = 0.3) +
    geom_line(
        aes(y = cover, color = "Coverage", size = "Current frame"),
        data = df_list[[1]]
    ) + geom_line(
        aes(y = overall, color = "Overall", size = "Current frame"),
        data = df_list[[1]]
    ) + geom_line(
        aes(y = cover, color = "Coverage", size = "5 frames"),
        data = df_list[[2]]
    ) + geom_line(
        aes(y = overall, color = "Overall", size = "5 frames"),
        data = df_list[[2]]
    ) +
    geom_line(aes(y = nact, color = "Active nodes"), size = 1) +
    geom_point(aes(y = nact, color = "Active nodes"), size = 2) +
    geom_line(aes(y = util, color = "Utility"), size = 1) +
    geom_point(aes(y = util, color = "Utility"), size = 2) +
    geom_line(aes(
            y = cover,
            color = "Coverage",
            size = "20 frames"
    )) + geom_point(aes(y = cover, color = "Coverage"), size = 2) +
    geom_line(aes(y = overall, color = "Overall"), size = 1) +
    geom_point(aes(y = overall, color = "Overall"), size = 2) +
    scale_y_continuous(
        sec.axis = sec_axis(~ . * 10000, name = "Data rate (byte / s)")
    ) +
    scale_color_manual(values = c(
        "Overall"           = "#333333",
        "Coverage"          = "#006633",
        "Utility"           = "#006666",
        "Active nodes"      = "#663300"
    )) + scale_fill_manual(values = c(
        "Data generation"   = "#cc0000"
    )) + scale_size_manual(values = c(
        "20 frames"         = 1.0,
        "5 frames"          = 0.5,
        "Current frame"     = 0.2
    )) + guides(
        color = guide_legend("Metrics"),
        fill = guide_legend(NULL),
        size = guide_legend("Temporal impact")
    )
ggsave(
    filename = "test_plot/batch_user_locations_static.png",
    plot = plot_obj,
    device = "png",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
ggsave(
    filename = "test_plot/batch_user_locations_static.pdf",
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
