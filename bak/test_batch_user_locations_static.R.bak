cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

require(methods)

# simulation CONSTANTS
num_cells_1 = 7L
num_cells_2 = 9L
cell_len_x = 500
cell_len_y = 500
num_offset_1 = -5 / cell_len_y
num_offset_2 = -5 / cell_len_x
lockBinding("num_cells_1", globalenv())
lockBinding("num_cells_2", globalenv())
lockBinding("num_offset_1", globalenv())
lockBinding("num_offset_2", globalenv())
lockBinding("cell_len_x", globalenv())
lockBinding("cell_len_y", globalenv())

num_nodes = 30L
num_cells = num_cells_1 * num_cells_2
num_types = 10L
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_types", globalenv())

num_static = seq(num_nodes, 0L, by = -3L)
lockBinding("num_static", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 3600L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

# mode-specific simulation SETTINGS
capacity_p = 0.8
lockBinding("capacity_p", globalenv())

rate_max = 50.0
lockBinding("rate_max", globalenv())

stopifnot(is.integer(num_cells))

data_file = "user_locations_RData/user_locations_yusuf_sample.RData"
num_rounds = length(num_static)
num_loops = 5L
t_impact_steps = seq(0L, 20L, by = 5L)

# CONSTRUCT test grid using parameters
source("lib/square_cell_grid.R")

grid = SquareCellGrid(
    num_cells_1,
    num_cells_2,
    cell_len_x,
    cell_len_y,
    num_offset_1,
    num_offset_2
)

# CREATE test case elements
source("lib/element_base.R")
source("lib/element_rand.R")

local_util_f = get_util_f_type("log_sum")

source("lib/placement_user_locations.R")

update_placement_f = update_placement_user_locations
get_placement_f = get_placement_user_locations

source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1

source("lib/objective_multi.R")

get_objective_f = get_objective_multi_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    t_imp_threshold = t_imp_threshold,
    rich_return = TRUE
)

# PREPARE data structures for simulation results
source("lib/simu_gamma.R")

objective_general_history = array(
    0,
    dim = c(
        num_rounds,
        length(objective_zero()),
        length(t_impact_steps),
        num_loops
    )
)
dimnames(objective_general_history)[[1]] = num_static
dimnames(objective_general_history)[[2]] = names(objective_zero())
dimnames(objective_general_history)[[3]] = t_impact_steps
dimnames(objective_general_history)[[4]] = z_nd_str("loop", num_loops)

objective_x_history = objective_u_history = array(
    0,
    dim = c(
        num_rounds,
        num_types,
        length(t_impact_steps),
        num_loops
    )
)
dimnames(objective_x_history)[[1]] = num_static
dimnames(objective_x_history)[[2]] = z_nd_str("d", num_types)
dimnames(objective_x_history)[[3]] = t_impact_steps
dimnames(objective_x_history)[[4]] = z_nd_str("loop", num_loops)
dimnames(objective_u_history) = dimnames(objective_x_history)

# RUN the simulation
for(pnd in 1L:num_loops) {
    # UPDATE simulation elements
    capacity_mat = get_capacity_mat_rand(
        val_n = num_nodes,
        val_k = num_types,
        p = capacity_p
    )
    data_type_spec_df = get_data_type_spec_df_rand(
        val_k = num_types,
        r_max = rate_max
    )
    make_s_impact_f_type(type = "step", val_k = num_types, step = 750)
    general_placement = create_placement_user_locations(
        t_frame = t_frame,
        duration = duration,
        val_n = num_nodes + num_static[1],
        val_m = num_cells,
        val_k = num_types,
        data_file = data_file,
        grid = grid,
        num_static = num_static[1]
    )

    # SIMULATION
    # objective_avg = array(
    #     0,
    #     dim = c(
    #         num_rounds,
    #         length(objective_zero()) + 1L,
    #         length(t_impact_steps)
    #     )
    # )
    # dimnames(objective_avg) = dimnames(objective_avg_sum)

    for(snd in 1L:length(t_impact_steps)) {
        make_t_impact_f_type(
            type = "step",
            val_k = num_types,
            step = t_impact_steps[snd]
        )

        for(rnd in 1L:num_rounds) {
            create_placement_f = get_recreate_placement_user_locations_f(
                num_static = num_static[rnd]
            )
            cat(
                "Simulation",
                sprintf("loop = %d of %d,", pnd, num_loops),
                sprintf("snd = %d of %d,", snd, length(t_impact_steps)),
                sprintf("rnd = %d of %d,", rnd, num_rounds),
                sprintf("num_static = %d", num_static[rnd]),
                "\n"
            )
            objective_list = simulate_gamma(
                t_frame                 = t_frame,
                duration                = duration,
                val_n                   = num_nodes,
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
                verbose                 = FALSE
            )
            objective_general_history[rnd, , snd, pnd] =
                                                   objective_list$general_avg
            objective_x_history[rnd, , snd, pnd] = objective_list$x_by_type_avg
            objective_u_history[rnd, , snd, pnd] = objective_list$u_by_type_avg
        }
    }
}

objective_general_avg = apply(
    objective_general_history,
    MARGIN = c(1, 2, 3),
    FUN = mean
)
objective_general_dev = apply(
    objective_general_history,
    MARGIN = c(1, 2, 3),
    FUN = sd
)
objective_x_avg = apply(
    objective_x_history,
    MARGIN = c(1, 2, 3),
    FUN = mean
)
objective_x_dev = apply(
    objective_x_history,
    MARGIN = c(1, 2, 3),
    FUN = sd
)
objective_u_avg = apply(
    objective_u_history,
    MARGIN = c(1, 2, 3),
    FUN = mean
)
objective_u_dev = apply(
    objective_u_history,
    MARGIN = c(1, 2, 3),
    FUN = sd
)

cat("\n")
cat("Objective(s)","\n")
print(list(
    general_avg = round(objective_general_avg[, , "5"], 4),
    general_dev = round(objective_general_dev[, , "5"], 4)
))
save(
    objective_general_history, objective_x_history, objective_u_history,
    objective_general_avg, objective_x_avg, objective_u_avg,
    objective_general_dev, objective_x_dev, objective_u_dev,
    num_cells_1, num_cells_2,
    cell_len_x, cell_len_y,
    num_offset_1, num_offset_2,
    num_nodes, num_cells, num_types, num_static,
    t_impact_steps,
    gamma_x, gamma_u, gamma_y,
    t_frame, duration,
    capacity_p, rate_max,
    data_file,
    num_rounds, num_loops,
    grid,
    local_util_f,
    calc_work_mat_f,
    file = "test_data/batch_user_locations_static.RData"
)
