rm(list = ls())

load("test_data/batch_user_locations_n.RData")

library(ggplot2)
library(reshape2)

num_loops = 5L
line_cols = c("nodes", "nact", "cover", "util", "overall")
df = data.frame(objective_general_avg)
df_se = data.frame(objective_general_dev) * qnorm(0.975) / sqrt(num_loops)
df$nodes = df_se$nodes = as.integer(rownames(objective_general_avg))
dm = melt(df[, line_cols], id.vars = "nodes", variable.name = "series")
dm_se = melt(df_se[, line_cols], id.vars = "nodes", variable.name = "series")
dm$se = dm_se$value
plot_obj = ggplot(data = dm, aes(x = nodes)) +
    xlab("Number of nodes") +
    ylab("Objectives") +
    expand_limits(y = 0) +
    geom_area(
        data = df,
        aes(y = traffic / 40000, fill = "traffic"),
    alpha = 0.3) + geom_line(
        aes(y = value, color = series), size = 1
    ) + geom_errorbar(
        aes(
            ymin = value - se,
            ymax = value + se,
            color = series
        ), size = 0.5
    ) + geom_point(
        aes(y = value, color = series, shape = series), size = 2
    ) + scale_y_continuous(
        sec.axis = sec_axis(
            ~ . * 40000,
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
    )
# plot_obj
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
