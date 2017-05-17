# eval_mob_a_src.R
#
# Created: 2017-5-17
#  Author: Charles Zhu
#
stopifnot(exists("gr"))
stopifnot(exists("data_type_spec_df"))
stopifnot(exists("t_frame"))
stopifnot(exists("duration"))
stopifnot(exists("calc_work_mat_f"))
stopifnot(exists("capacity_data_format") && is.character(capacity_data_format))
stopifnot(exists("mobility_data_format") && is.character(mobility_data_format))
stopifnot(exists("save_to_file") && is.character(save_to_file))

if(!exists("num_loops")) num_loops <<- 5L
num_rounds <<- length(num_nodes)

cat("Evaluating...", "\n")

suppressPackageStartupMessages(require(methods))

# CREATE test case elements
num_mob <<- num_nodes - num_static

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

# PREPARE data structures for simulation results
source("lib/basic.R")

objective_general_history = array(
    0,
    dim = c(num_rounds, length(objective_zero()), num_loops)
)
dimnames(objective_general_history) = rep(list(NULL), 3L)
dimnames(objective_general_history)[[1]] = num_mob
dimnames(objective_general_history)[[2]] = names(objective_zero())
dimnames(objective_general_history)[[3]] = z_nd_str("loop", num_loops)

objective_x_history = array(
    0,
    dim = c(num_rounds, num_types, num_loops)
)
dimnames(objective_x_history) = rep(list(NULL), 3L)
dimnames(objective_x_history)[[1]] = num_mob
dimnames(objective_x_history)[[2]] = z_nd_str("d", num_types)
dimnames(objective_x_history)[[3]] = z_nd_str("loop", num_loops)

objective_u_history = array(
    0,
    dim = c(num_rounds, num_types, num_loops)
)
dimnames(objective_u_history) = dimnames(objective_x_history)

proc_t_general_history = array(
    0,
    dim = c(num_rounds, 2L, num_loops)
)
dimnames(proc_t_general_history) = rep(list(NULL), 3L)
dimnames(proc_t_general_history)[[1]] = num_mob
dimnames(proc_t_general_history)[[2]] = c("cal", "obj")
dimnames(proc_t_general_history)[[3]] = z_nd_str("loop", num_loops)

# RUN the simulation
source("lib/simu_gamma.R")

for(pnd in 1L:num_loops) {
    load(sprintf(capacity_data_format, pnd))
    load(sprintf(mobility_data_format, pnd))

    # SIMULATION
    for(rnd in 1L:num_rounds) {
        if(num_mob[rnd] > 0L) {
            capacity_mat = rbind(
                capacity_mobile[1L:num_mob[rnd], ],
                capacity_static[1L:num_static[rnd], ]
            )
        } else capacity_mat = capacity_static[1L:num_static[rnd], ]
        create_placement_f = get_recreate_placement_user_locations_f(
            num_static = num_static[rnd]
        )
        cat(
            "Simulation",
            sprintf("loop = %d of %d,", pnd, num_loops),
            sprintf("rnd = %d of %d,", rnd, num_rounds),
            sprintf("num_mob = %d", num_mob[rnd]),
            "\n"
        )
        objective_list = simulate_gamma(
            t_frame                 = t_frame,
            duration                = duration,
            val_n                   = num_nodes[rnd],
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
            verbose                 = FALSE
        )
        objective_general_history[rnd, , pnd]   = objective_list$general_avg
        objective_x_history[rnd, , pnd]         = objective_list$x_by_type_avg
        objective_u_history[rnd, , pnd]         = objective_list$u_by_type_avg
        proc_t_general_history[rnd, , pnd]      = objective_list$proc_t_avg
    }
}

objective_general_avg   = apply(objective_general_history,
                                MARGIN = c(1, 2), FUN = mean)
objective_general_dev   = apply(objective_general_history,
                                MARGIN = c(1, 2), FUN = sd)
objective_x_avg         = apply(objective_x_history,
                                MARGIN = c(1, 2), FUN = mean)
objective_x_dev         = apply(objective_x_history,
                                MARGIN = c(1, 2), FUN = sd)
objective_u_avg         = apply(objective_u_history,
                                MARGIN = c(1, 2), FUN = mean)
objective_u_dev         = apply(objective_u_history,
                                MARGIN = c(1, 2), FUN = sd)
proc_t_general_avg      = apply(proc_t_general_history,
                                MARGIN = c(1, 2), FUN = mean)
proc_t_general_dev      = apply(proc_t_general_history,
                                MARGIN = c(1, 2), FUN = sd)

cat("\n")
cat("Objective(s)","\n")
print(list(
    general_avg = round(objective_general_avg, 4),
    general_dev = round(objective_general_dev, 4)
))
save(
    objective_general_history, objective_x_history, objective_u_history,
    objective_general_avg, objective_x_avg, objective_u_avg,
    objective_general_dev, objective_x_dev, objective_u_dev,
    proc_t_general_history, proc_t_general_avg, proc_t_general_dev,
    num_cells_1, num_cells_2,
    cell_len_x, cell_len_y,
    num_offset_1, num_offset_2,
    num_nodes, num_cells, num_types, num_static,
    val_k_gas, val_k_audio, val_k_photo, val_k_wifi, weight_types,
    num_gas_mob, p_audio_mob, p_photo_mob, p_photo_static,
    gamma_x, gamma_u, gamma_y,
    t_frame, duration,
    data_file,
    num_rounds, num_loops,
    gr,
    local_util_f,
    calc_work_mat_f,
    file = save_to_file
)
