# plot_res_mappings.R
#
# Created: 2017-7-16
#  Author: Charles Zhu
#
pa_obj_name                 = "Metrics"
lockBinding("pa_obj_name", globalenv())

pa_obj_label                = c(
    "overall"               = "Overall performance",
    "traffic"               = "Data generation (byte / sec)",
    "cover"                 = "Coverage",
    "util"                  = "Utility",
    "nact"                  = "Active nodes normalized"
)
lockBinding("pa_obj_label", globalenv())

pa_sol_name                 = "Solutions"
lockBinding("pa_sol_name", globalenv())

pa_sol_label                = c(
    "fill_1_%s"             = "Everything",
    "greedy_1_%s_quota"     = "HSF",
    "greedy_2_%s_quota"     = "HSF-ST",
    "greedy_2_%s_full"      = "HSF-ST Inf",
    "ga_1_%s_quota"         = "GA",
    "lyap_grd_%s_quota"     = "Lyapunov",
    "lyap_grd_%s_full"      = "Lyapunov Inf"
)
pa_sol_linetype             = c(
    "fill_1_%s"             = "11",
    "greedy_1_%s_quota"     = "solid",
    "greedy_2_%s_quota"     = "solid",
    "greedy_2_%s_full"      = "11",
    "ga_1_%s_quota"         = "solid",
    "lyap_grd_%s_quota"     = "solid",
    "lyap_grd_%s_full"      = "11"
)
pa_sol_alpha                = c(
    "fill_1_%s"             = 1,
    "greedy_1_%s_quota"     = 0.5,
    "greedy_2_%s_quota"     = 1,
    "greedy_2_%s_full"      = 0.5,
    "ga_1_%s_quota"         = 0.5,
    "lyap_grd_%s_quota"     = 1,
    "lyap_grd_%s_full"      = 0.5
)
lockBinding("pa_sol_label", globalenv())
lockBinding("pa_sol_linetype", globalenv())
lockBinding("pa_sol_alpha", globalenv())

pa_sol_shape =
pp_sol_shape                = c(
    "fill_1_%s"             = 1,
    "greedy_1_%s_quota"     = 6,
    "greedy_2_%s_quota"     = 7,
    "greedy_2_%s_full"      = 13,
    "ga_1_%s_quota"         = 9,
    "lyap_grd_%s_quota"     = 12,
    "lyap_grd_%s_full"      = 10
)
lockBinding("pp_sol_shape", globalenv())

pa_sol_color                = c(
    "fill_1_%s"             = "gray15",
    "greedy_1_%s_quota"     = "violetred3",
    "greedy_2_%s_quota"     = "blue2",
    "greedy_2_%s_full"      = "blue4",
    "ga_1_%s_quota"         = "salmon3",
    "lyap_grd_%s_quota"     = "darkorchid2",
    "lyap_grd_%s_full"      = "darkorchid4"
)
lockBinding("pa_sol_color", globalenv())

library(ggplot2)

res_themes =  theme_light() +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 18)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.text = element_text(color = "gray30")) +
    theme(legend.title = element_text(size = 18)) +
    theme(legend.background = element_rect(
            linetype = "solid", color = "gray30"
        )
    )
lockBinding("res_themes", globalenv())
