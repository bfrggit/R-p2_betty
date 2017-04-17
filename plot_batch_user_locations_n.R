rm(list = ls())

load("test_data/batch_user_locations_n.RData")

plot_obj = ggplot(data = data.frame(objective_avg_avg), aes(x = nodes)) +
    xlab("Number of nodes") +
    ylab("Objective value") +
    geom_area(aes(y = traffic / 40000, fill = "Data generation"), alpha = 0.3) +
    geom_line(aes(y = nact, color = "Active nodes"), size = 1) +
    geom_point(aes(y = nact, color = "Active nodes"), size = 2) +
    geom_line(aes(y = util, color = "Utility"), size = 1) +
    geom_point(aes(y = util, color = "Utility"), size = 2) +
    geom_line(aes(y = cover, color = "Coverage"), size = 1) +
    geom_point(aes(y = cover, color = "Coverage"), size = 2) +
    geom_line(aes(y = overall, color = "Overall"), size = 1) +
    geom_point(aes(y = overall, color = "Overall"), size = 2) +
    scale_y_continuous(
        sec.axis = sec_axis(~ . * 40000, name = "Data rate (byte / s)")
    ) +
    scale_color_manual(values = c(
        "Overall"           = "#333333",
        "Coverage"          = "#006633",
        "Utility"           = "#006666",
        "Active nodes"      = "#663300"
    )) + scale_fill_manual(values = c(
        "Data generation"   = "#cc0000"
    )) + guides(color = guide_legend(NULL), fill = guide_legend(NULL))
plot_obj
