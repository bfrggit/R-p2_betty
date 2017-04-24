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

num_nodes = 100L
num_cells = num_cells_1 * num_cells_2
num_types = 10L
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())
lockBinding("num_types", globalenv())

num_static = 60L
lockBinding("num_static", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 10800L
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
source("lib/element_base.R")
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
make_s_impact_f_type(
    type = "step",
    val_k = num_types,
    step = 750
)
make_t_impact_f_type(
    type = "step",
    val_k = num_types,
    step = 5
)
local_util_f = get_util_f_type("max")

source("lib/placement_user_locations.R")

create_placement_f = get_create_placement_user_locations_f(
    data_file = "user_locations_RData/user_locations_yusuf_sample.RData",
    grid = grid,
    num_static = num_static
)

# test_placement = create_placement_f(
#     t_frame = t_frame,
#     duration = duration,
#     val_n = num_nodes,
#     val_m = num_cells,
#     val_k = num_types
# ) # TEST

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

# RUN the simulation
source("lib/simu_gamma.R")

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
    verbose                 = TRUE
)

# VALIDATE average of cover and util
# this implementation often fails due to double float accuracy
# stopifnot(objective_list$general_avg["cover"] ==
#     sum(objective_list$x_by_type_avg * data_type_spec_df[, "weight"])
# )
# stopifnot(objective_list$general_avg["util"] ==
#     sum(objective_list$u_by_type_avg * data_type_spec_df[, "weight"])
# )


# VALIDATE average of cover and util
stopifnot(
    abs(objective_list$general_avg["cover"] -
        sum(objective_list$x_by_type_avg * data_type_spec_df[, "weight"])
    ) < 1e-8
)
stopifnot(
    abs(objective_list$general_avg["util"] -
        sum(objective_list$u_by_type_avg * data_type_spec_df[, "weight"])
    ) < 1e-8
)

cat("\n")
cat("Objective(s)","\n")
print(lapply(objective_list, FUN = function(x) {round(x, 4)}))
