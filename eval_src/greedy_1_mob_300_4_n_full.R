cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

require(methods)

source("lib/square_cell_grid.R")
source("lib/element_multi.R")
source("lib/placement_user_locations.R")

# simulation CONSTANTS
load("prep_RData/grid_mob_one.RData")
lockBinding("grid", globalenv())

num_nodes = c(seq(5L, 25L, by = 5L), seq(30L, 160L, by = 10L))
num_cells = num_cells_1 * num_cells_2
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())

num_static = as.integer(num_nodes * 0.6)
lockBinding("num_static", globalenv())

data_quota = +Inf
lockBinding("data_quota", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1.2

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 10800L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

num_rounds = length(num_nodes)
num_loops = 5L

# mode-specific simulation SETTINGS
load("prep_RData/impact_multi.RData")
lockBinding("data_type_spec_df", globalenv())

stopifnot(is.integer(num_cells))

# CREATE test case elements
num_mob = num_nodes - num_static
local_util_f = get_util_f_type("log_sum")

update_placement_f = update_placement_user_locations
get_placement_f = get_placement_user_locations

source("lib/calc_work_fill_1.R")
source("lib/objective_multi.R")

calc_work_mat_f = get_calc_work_mat_greedy_1_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y
)
get_objective_f = get_objective_multi_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    t_imp_threshold = t_imp_threshold,
    rich_return = TRUE,
    eval_impact = FALSE
)
ret_eval_impact_mat = evaluate_impact_mat(
    duration_frames = duration %/% t_frame,
    val_m           = num_cells,
    val_k           = num_types,
    grid            = grid,
    data_type_specs = data_type_spec_df
)

source("lib/basic.R")
source("lib/simu_gamma.R")

# PREPARE data structures for simulation results
objective_general_history = array(
    0,
    dim = c(num_rounds, length(objective_zero()), num_loops)
)
dimnames(objective_general_history)[[1]] = num_nodes
dimnames(objective_general_history)[[2]] = names(objective_zero())
dimnames(objective_general_history)[[3]] = z_nd_str("loop", num_loops)

objective_x_history = array(
    0,
    dim = c(num_rounds, num_types, num_loops)
)
dimnames(objective_x_history)[[1]] = num_nodes
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
dimnames(proc_t_general_history)[[1]] = num_nodes
dimnames(proc_t_general_history)[[2]] = c("cal", "obj")
dimnames(proc_t_general_history)[[3]] = z_nd_str("loop", num_loops)

# RUN the simulation
for(pnd in 1L:num_loops) {
    load(sprintf("prep_RData/impact_multi_capacity_sm_%d.RData", pnd))
    load(sprintf("prep_RData/mob_300_4_%d.RData", pnd))

    # SIMULATION
    for(rnd in 1L:num_rounds) {
        capacity_mat = rbind(
            capacity_mobile[1L:num_mob[rnd], ],
            capacity_static[1L:num_static[rnd], ]
        )
        create_placement_f = get_recreate_placement_user_locations_f(
            num_static = num_static[rnd]
        )
        cat(
            "Simulation",
            sprintf("loop = %d of %d,", pnd, num_loops),
            sprintf("rnd = %d of %d,", rnd, num_rounds),
            sprintf("num_nodes = %d", num_nodes[rnd]),
            "\n"
        )
        objective_list = simulate_gamma(
            t_frame                 = t_frame,
            duration                = duration,
            val_n                   = num_nodes[rnd],
            val_m                   = num_cells,
            val_k                   = num_types,
            grid                    = grid,
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
        objective_general_history[rnd, , pnd] = objective_list$general_avg
        objective_x_history[rnd, , pnd] = objective_list$x_by_type_avg
        objective_u_history[rnd, , pnd] = objective_list$u_by_type_avg
        proc_t_general_history[rnd, , pnd] = objective_list$proc_t_avg
    }
}

objective_general_avg = apply(objective_general_history,
    MARGIN = c(1, 2), FUN = mean)
objective_general_dev = apply(objective_general_history,
    MARGIN = c(1, 2), FUN = sd)
objective_x_avg = apply(objective_x_history,
    MARGIN = c(1, 2), FUN = mean)
objective_x_dev = apply(objective_x_history,
    MARGIN = c(1, 2), FUN = sd)
objective_u_avg = apply(objective_u_history,
    MARGIN = c(1, 2), FUN = mean)
objective_u_dev = apply(objective_u_history,
    MARGIN = c(1, 2), FUN = sd)
proc_t_general_avg = apply(proc_t_general_history, MARGIN = c(1, 2), FUN = mean)
proc_t_general_dev = apply(proc_t_general_history, MARGIN = c(1, 2), FUN = sd)

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
    grid,
    local_util_f,
    calc_work_mat_f,
    file = "eval_data/greedy_1_mob_300_4_n_full.RData"
)
