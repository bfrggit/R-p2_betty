cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

# simulation CONSTANTS
num_cells_1 = 8L
num_cells_2 = 8L
num_offset_1 = -2L
num_offset_2 = 2L
cell_len_x = 100
cell_len_y = 100
lockBinding("num_cells_1", globalenv())
lockBinding("num_cells_2", globalenv())
lockBinding("num_offset_1", globalenv())
lockBinding("num_offset_2", globalenv())
lockBinding("cell_len_x", globalenv())
lockBinding("cell_len_y", globalenv())

num_nodes = 20L
num_cells = num_cells_1 * num_cells_2
num_types = 10L
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_types", globalenv())

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
source("lib/element_rand.R")

capacity_mat = get_capacity_mat_rand(
    val_n = num_nodes,
    val_k = num_types,
    p = capacity_p
)
data_type_spec_df = get_data_type_spec_df_rand(
    val_k = num_types,
    r_max = rate_max
)
make_impact_f_local_only(val_k = num_types)

source("lib/placement_rand.R")

create_placement_f = create_placement_rand
update_placement_f = update_placement_rand
get_placement_f = get_placement_rand

source("lib/calc_work_fill_1.R")

calc_work_mat_f = calc_work_mat_fill_1

source("lib/objective_multi.R")

get_objective_f = get_objective_zero_f()

# RUN the simulation
source("lib/simu_beta.R")

objective_avg = simulate_beta(
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
    verbose                 = TRUE
)

cat("\n")
cat("Objective(s)","\n")
print(objective_avg)
