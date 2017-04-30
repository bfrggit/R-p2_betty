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

num_nodes = c(seq(5L, 25L, by = 5L), seq(30L, 160L, by = 10L))
num_cells = num_cells_1 * num_cells_2
num_types = 10L
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_types", globalenv())

num_static = as.integer(num_nodes * 0.6)
lockBinding("num_static", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 7200L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

# mode-specific simulation SETTINGS
val_k_gas   = 6L
val_k_audio = 1L
val_k_photo = 2L
val_k_wifi  = 1L
lockBinding("val_k_gas", globalenv())
lockBinding("val_k_audio", globalenv())
lockBinding("val_k_photo", globalenv())
lockBinding("val_k_wifi", globalenv())

num_gas_mob = 4L
lockBinding("num_gas_mob", globalenv())

p_audio_mob = c(audio_1 = 0.6)
p_photo_mob = c(photo_1 = 0.9, photo_2 = 0.3)
p_photo_static = c(photo_1 = 0, photo_2 = 0.7)
lockBinding("p_audio_mob", globalenv())
lockBinding("p_photo_mob", globalenv())
lockBinding("p_photo_static", globalenv())

weight_types = c(
    c(      # gas
        8,
        5,
        5,
        4,
        4,
        3
    ), c(   # audio
        5
    ), c(   # photo
        7,
        6
    ), c(   # wifi
        3
    )
)
names(weight_types) = paste("d", 1L:num_types, sep = "_")
lockBinding("weight_types", globalenv())

stopifnot(is.integer(num_cells))

get_t_imp_f = function(t_const) {as.integer(ceiling(t_const * 60 / t_frame))}
col_type_lt = cumsum(c(1L, val_k_gas, val_k_audio, val_k_photo))
col_type_rt = cumsum(c(val_k_gas, val_k_audio, val_k_photo, val_k_wifi))

data_file = "user_locations_RData/user_locations_yusuf_sample.RData"
num_rounds = length(num_nodes)
num_loops = 5L

num_mob = num_nodes - num_static
num_nodes_all = num_nodes[num_rounds]
num_static_all = num_static[num_rounds]
num_mob_all = num_nodes_all - num_static_all

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
source("lib/element_multi.R")

data_type_spec_df = get_data_type_spec_df_multi(
    val_k_gas       = val_k_gas,
    val_k_audio     = val_k_audio,
    val_k_photo     = val_k_photo,
    val_k_wifi      = val_k_wifi,
    weight_original = weight_types
)
make_s_impact_f_type_vec(kcl = col_type_lt[1]:col_type_rt[1],
                         type = "exp", t_const = 550
)
make_s_impact_f_type_vec(kcl = col_type_lt[2]:col_type_rt[2],
                         type = "exp", t_const = 100
)
make_s_impact_f_type_vec(kcl = col_type_lt[3],
                         type = "exp", t_const = 10
)
make_s_impact_f_type_vec(kcl = col_type_lt[3] + 1L,
                         type = "exp", t_const = 750
)
make_s_impact_f_type_vec(kcl = col_type_lt[4]:col_type_rt[4],
                         type = "exp", t_const = 20
)
make_t_impact_f_type_vec(kcl = col_type_lt[1]:col_type_rt[1],
                         type = "step", step = get_t_imp_f(20)
)
make_t_impact_f_type_vec(kcl = col_type_lt[2]:col_type_rt[2],
                         type = "step", step = get_t_imp_f(5)
)
make_t_impact_f_type_vec(kcl = col_type_lt[3],
                         type = "step", step = get_t_imp_f(10)
)
make_t_impact_f_type_vec(kcl = col_type_lt[3] + 1L,
                         type = "step", step = get_t_imp_f(30)
)
make_t_impact_f_type_vec(kcl = col_type_lt[4]:col_type_rt[4],
                         type = "step", step = get_t_imp_f(60)
)
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

# RUN the simulation
for(pnd in 1L:num_loops) {
    # UPDATE simulation elements
    capacity_mat_of_all = get_capacity_mat_multi(
        val_n           = num_nodes[num_rounds],
        num_static      = num_static[num_rounds],
        val_k_gas       = val_k_gas,
        val_k_audio     = val_k_audio,
        val_k_photo     = val_k_photo,
        val_k_wifi      = val_k_wifi,
        num_gas_mob     = num_gas_mob,
        p_audio_mob     = p_audio_mob,
        p_photo_mob     = p_photo_mob,
        p_photo_static  = p_photo_static
    )
    # data_type_spec_df = get_data_type_spec_df_rand(
    #     val_k = num_types,
    #     r_max = rate_max
    # )
    # make_s_impact_f_type(type = "step", val_k = num_types, step = 750)
    # make_t_impact_f_type(type = "step", val_k = num_types, step = 5)
    general_placement = create_placement_user_locations(
        t_frame = t_frame,
        duration = duration,
        val_n = num_nodes[num_rounds],
        val_m = num_cells,
        val_k = num_types,
        data_file = data_file,
        grid = grid,
        num_static = num_static[num_rounds]
    )

    # SIMULATION
    for(rnd in 1L:num_rounds) {
        capacity_mat = matrix(
            0,
            ncol = ncol(capacity_mat_of_all),
            nrow = num_nodes[rnd]
        )
        colnames(capacity_mat) = colnames(capacity_mat_of_all)
        rownames(capacity_mat) = paste("n", 1L:num_nodes[rnd], sep = "_")
        capacity_mat[1L:num_mob[rnd], ] = capacity_mat_of_all[1L:num_mob[rnd], ]
        capacity_mat[(num_mob[rnd] + 1L):num_nodes[rnd], ] =
            capacity_mat_of_all[
                (num_mob_all + 1L):(num_mob_all + num_static[rnd]),
            ]
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
            verbose                 = FALSE
        )
        objective_general_history[rnd, , pnd] = objective_list$general_avg
        objective_x_history[rnd, , pnd] = objective_list$x_by_type_avg
        objective_u_history[rnd, , pnd] = objective_list$u_by_type_avg
    }
}

objective_general_avg = apply(
    objective_general_history,
    MARGIN = c(1, 2),
    FUN = mean
)
objective_general_dev = apply(
    objective_general_history,
    MARGIN = c(1, 2),
    FUN = sd
)
objective_x_avg = apply(
    objective_x_history,
    MARGIN = c(1, 2),
    FUN = mean
)
objective_x_dev = apply(
    objective_x_history,
    MARGIN = c(1, 2),
    FUN = sd
)
objective_u_avg = apply(
    objective_u_history,
    MARGIN = c(1, 2),
    FUN = mean
)
objective_u_dev = apply(
    objective_u_history,
    MARGIN = c(1, 2),
    FUN = sd
)

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
    file = "test_data/batch_multi_n.RData"
)
