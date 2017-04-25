rm(list = ls())

load("test_data/batch_user_locations_n.RData")

library(ggplot2)
library(reshape2)

num_loops = 5L
df = data.frame(objective_u_avg)
df$nodes = as.integer(rownames(objective_u_avg))
dm = melt(df, id.vars = "nodes", variable.name = "series")
plot_obj = ggplot(data = dm, aes(x = nodes)) +
    xlab("Number of nodes") +
    ylab("Coverage") +
    expand_limits(y = 0) +
    geom_point(aes(y = value, color = series, shape = series), size = 2) +
    geom_line(aes(y = value, color = series), size = 1) +
    scale_color_hue(
        name = "Data types",
        labels = paste("Type", 1L:ncol(objective_x_avg), sep = " "),
        h = c(-15, 135), c = 100, l = 55, direction = -1
    ) + scale_shape_manual(
        name = "Data types",
        labels = paste("Type", 1L:ncol(objective_x_avg), sep = " "),
        values = 1L:ncol(objective_x_avg) - 1L
    )
# plot_obj
ggsave(
    filename = "test_plot/batch_user_locations_n_obj_u.pdf",
    plot = plot_obj,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
)
