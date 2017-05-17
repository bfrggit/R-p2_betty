# settings.R
#
# Created: 2017-5-16
#  Author: Charles Zhu
#
source("lib/square_cell_grid.R")
source("lib/element_multi.R")

# simulation CONSTANTS
if(!exists("gr")) {
    load("prep_RData/grid_mob_one.RData")
    lockBinding("gr", globalenv())
}
num_cells_1 = gr@num_cells_1
num_cells_2 = gr@num_cells_2

if(!exists("num_nodes"))        num_nodes <<- 100L
if(!exists("num_cells"))        num_cells <<- num_cells_1 * num_cells_2
if(!exists("num_static"))       num_static <<- as.integer(num_nodes * 0.6)
if(!exists("data_quota"))       data_quota <<- +Inf
if(!exists("gamma_x"))          gamma_x <<- 1
if(!exists("gamma_u"))          gamma_u <<- 0.4 / log(2)
if(!exists("gamma_y"))          gamma_y <<- -1.2
if(!exists("t_imp_threshold"))  t_imp_threshold <<- 1e-4
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_static", globalenv())
lockBinding("data_quota", globalenv())
lockBinding("gamma_x", globalenv())
lockBinding("gamma_u", globalenv())
lockBinding("gamma_y", globalenv())
stopifnot(is.integer(num_cells))

# general simulation SETTINGS
if(!exists("t_frame"))          t_frame <<- 60L
if(!exists("duration"))         duration <<- 10800L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

# mode-specific simulation SETTINGS
if(!exists("data_type_spec_df")) {
    load("prep_RData/impact_multi.RData")
    lockBinding("data_type_spec_df", globalenv())
}

if(!exists("local_util_f"))     local_util_f <<- get_util_f_type("log_sum")

# PREPARE placement func
source("lib/placement_user_locations.R")

create_placement_f <<- get_recreate_placement_user_locations_f(num_static)
update_placement_f <<- get_update_placement_user_locations_f(gr)
get_placement_f <<- get_placement_user_locations
