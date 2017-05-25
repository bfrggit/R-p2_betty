# plot_mappings.R
#
# Created: 2017-5-24
#  Author: Charles Zhu
#
pa_obj_name                 = "Metrics"
lockBinding("pa_obj_name", globalenv())

pa_obj_label                = c(
    "overall"               = "Overall performance",
    "trf_n"                 = "Data generation",
    "cover"                 = "Coverage",
    "util"                  = "Utility",
    "nact"                  = "Active nodes"
)
pa_obj_color                = c(
    "overall"               = "gray15",
    "trf_n"                 = "indianred",
    "cover"                 = "dodgerblue4",
    "util"                  = "purple4",
    "nact"                  = "orangered3"
)
pa_obj_shape                = c(
    "overall"               = 16,
    "trf_n"                 = 8,
    "cover"                 = 15,
    "util"                  = 17,
    "nact"                  = 18
)
lockBinding("pa_obj_label", globalenv())
lockBinding("pa_obj_color", globalenv())
lockBinding("pa_obj_shape", globalenv())

pa_sol_name                 = "Solutions"
lockBinding("pa_sol_name", globalenv())

pa_sol_label                = c(
    "fill_1_%s"             = "Everything",
    "greedy_1_%s_quota"     = "HSF",
    "greedy_2_%s_quota"     = "HSF-ST"
)
pa_sol_linetype             = c(
    "fill_1_%s"             = "11",
    "greedy_1_%s_quota"     = "42",
    "greedy_2_%s_quota"     = "solid"
)
pa_sol_alpha                = c(
    "fill_1_%s"             = 1,
    "greedy_1_%s_quota"     = 0.5,
    "greedy_2_%s_quota"     = 1
)
lockBinding("pa_sol_label", globalenv())
lockBinding("pa_sol_linetype", globalenv())
lockBinding("pa_sol_alpha", globalenv())

pp_sol_shape                = c(
    "fill_1_%s"             = 1,
    "greedy_1_%s_quota"     = 6,
    "greedy_2_%s_quota"     = 7
)
lockBinding("pp_sol_shape", globalenv())
