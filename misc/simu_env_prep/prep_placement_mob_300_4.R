rm(list = ls())

# simulation CONSTANTS
num_cells_1 = 7L
num_cells_2 = 9L
cell_len_x = 500
cell_len_y = 500
num_offset_1 = -5 / cell_len_y
num_offset_2 = -5 / cell_len_x

# general simulation SETTINGS
t_frame = 60L
duration = 10800L
num_nodes = 1800L
num_cells = num_cells_1 * num_cells_2
num_types = 1L # not necessary
num_static = 1200L
data_file = "traces_RData/mob_300_4.RData"

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

source("lib/placement_user_locations.R")

seeds = 1L:10L
for(seed in seeds) {
    cat(
        sprintf("Make random placement seed = %d\n", seed)
    )
    set.seed(seed)
    general_placement = create_placement_user_locations(
        t_frame = t_frame,
        duration = duration,
        val_n = num_nodes,
        val_m = num_cells,
        val_k = num_types,
        data_file = data_file,
        grid = grid,
        num_static = num_static
    )
    save(
        user_locations_time_bases,
        user_locations_id_map,
        user_locations_list_traces,
        user_locations_static,
        data_file,
        file = sprintf("placement_RData/mob_300_4_%d.RData", seed)
    )
}
