cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

# simulation CONSTANTS
num_cells_1 = 40L
num_cells_2 = 40L
num_offset_1 = -10L
num_offset_2 = 10L
cell_len_x = 50
cell_len_y = 50
lockBinding("num_cells_1", globalenv())
lockBinding("num_cells_2", globalenv())
lockBinding("num_offset_1", globalenv())
lockBinding("num_offset_2", globalenv())
lockBinding("cell_len_x", globalenv())
lockBinding("cell_len_y", globalenv())

num_nodes = 10L
num_cells = num_cells_1 * num_cells_2
num_types = 10L
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_types", globalenv())

# mode-specific simulation SETTINGS
capacity_p = 0.8
lockBinding("capacity_p", globalenv())

rate_max = 40.0
lockBinding("rate_max", globalenv())

stopifnot(is.integer(num_cells))

source("lib/square_cell_grid.R")
source("lib/element_rand.R")
source("lib/calc_work_fill_1.R")

# CONSTRUCT test grid using parameters
grid = SquareCellGrid(
    num_cells_1,
    num_cells_2,
    cell_len_x,
    cell_len_y,
    num_offset_1,
    num_offset_2
)

# CREATE test case elements
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
