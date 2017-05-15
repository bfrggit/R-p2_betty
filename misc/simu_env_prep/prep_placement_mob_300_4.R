# prep_placement_mob_300_4.R
#
# Created: 2017-5-14
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

load("prep_RData/grid_mob_one.RData")

# general simulation SETTINGS
t_frame = 60L
duration = 1L # not necessary
num_nodes = 1800L
num_cells = num_cells_1 * num_cells_2
num_types = 1L # not necessary
num_static = 1200L
data_file = "traces_RData/mob_300_4.RData"

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
        file = sprintf("prep_RData/mob_300_4_%d.RData", seed)
    )
}
