# test_src.R
#
# Created: 2017-5-16
#  Author: Charles Zhu
#
stopifnot(exists("gr"))
stopifnot(exists("data_type_spec_df"))
stopifnot(exists("t_frame"))
stopifnot(exists("duration"))
stopifnot(exists("capacity_mobile"))
stopifnot(exists("capacity_static"))
stopifnot(exists("user_locations_list_traces"))
stopifnot(exists("user_locations_static"))
stopifnot(exists("calc_work_mat_f"))

cat("Testing...", "\n")

suppressPackageStartupMessages(require(methods))

# CREATE test case elements
num_mob <<- num_nodes - num_static
capacity_mat <<- rbind(
    capacity_mobile[1L:num_mob, ],
    capacity_static[1L:num_static, ]
)

source("lib/objective_multi.R")

get_objective_f <<- get_objective_multi_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    t_imp_threshold = t_imp_threshold,
    rich_return = TRUE,
    eval_impact = FALSE
)
ret_eval_impact_mat <<- evaluate_impact_mat(
    duration_frames = duration %/% t_frame,
    val_m           = num_cells,
    val_k           = num_types,
    grid            = gr,
    data_type_specs = data_type_spec_df
)

# RUN the simulation
source("lib/simu_gamma.R")

objective_list <<- simulate_gamma(
    t_frame                 = t_frame,
    duration                = duration,
    val_n                   = num_nodes,
    val_m                   = num_cells,
    val_k                   = num_types,
    grid                    = gr,
    data_type_specs         = data_type_spec_df,
    capacity_mat            = capacity_mat,
    create_placement_f      = create_placement_f,
    update_placement_f      = update_placement_f,
    get_placement_f         = get_placement_f,
    calc_work_mat_f         = calc_work_mat_f,
    get_objective_f         = get_objective_f,
    local_util_f            = local_util_f,
    data_quota              = data_quota,
    verbose                 = TRUE
)

# VALIDATE average of cover and util
stopifnot(
    abs(objective_list$general_avg["cover"] -
        sum(objective_list$x_by_type_avg * data_type_spec_df$weight)
    ) < 1e-8
)
stopifnot(
    abs(objective_list$general_avg["util"] -
        sum(objective_list$u_by_type_avg * data_type_spec_df$weight)
    ) < 1e-8
)

cat("\n")
cat("Objective(s)","\n")
print(lapply(objective_list, FUN = function(x) {round(x, 4)}))

if(exists("save_to_file") && is.character(save_to_file)) {
    save(
        obj_x_history, obj_u_history, proc_t_history,
        objective_list, objective_history, objective_avg_history,
        num_cells_1, num_cells_2,
        cell_len_x, cell_len_y,
        num_offset_1, num_offset_2,
        num_nodes, num_cells, num_types, num_static,
        val_k_gas, val_k_audio, val_k_photo, val_k_wifi, weight_types,
        num_gas_mob, p_audio_mob, p_photo_mob, p_photo_static,
        gamma_x, gamma_u, gamma_y,
        t_frame, duration,
        data_file,
        gr,
        local_util_f,
        calc_work_mat_f,
        file = save_to_file
    )
}
