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

num_nodes = 100L
num_cells = num_cells_1 * num_cells_2
lockBinding("num_nodes", globalenv())
lockBinding("num_cells", globalenv())

num_static = as.integer(num_nodes * 0.6) # 60L
lockBinding("num_static", globalenv())

data_quota = +Inf
lockBinding("data_quota", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1.5

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 180L # 10800L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

# mode-specific simulation SETTINGS
load("prep_RData/impact_multi.RData")
lockBinding("data_type_spec_df", globalenv())

stopifnot(is.integer(num_cells))

# CREATE test case elements
num_mob = num_nodes - num_static
load("prep_RData/impact_multi_capacity_sm_4.RData")
capacity_mat = rbind(
    capacity_mobile[1L:num_mob, ],
    capacity_static[1L:num_static, ]
)
local_util_f = get_util_f_type("log_sum")

load("prep_RData/mob_300_4_4.RData")
create_placement_f = get_recreate_placement_user_locations_f(num_static)
update_placement_f = update_placement_user_locations
get_placement_f = get_placement_user_locations

source("solution/greedy_1.R")
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
    data_quota              = data_quota,
    verbose                 = TRUE
)

# VALIDATE average of cover and util
stopifnot(
    abs(objective_list$general_avg["cover"] -
        sum(objective_list$x_by_type_avg * data_type_spec_df$weight)
    ) < 1e-8
)
stopifnot(
    abs(objective_list$general_avg["util"] -
        sum(objective_list$u_by_type_avg * data_type_spec_df$weight)
    ) < 1e-8
)

cat("\n")
cat("Objective(s)","\n")
print(lapply(objective_list, FUN = function(x) {round(x, 4)}))
save(
    obj_x_history, obj_u_history, proc_t_history,
    objective_list, objective_history, objective_avg_history,
    num_cells_1, num_cells_2,
    cell_len_x, cell_len_y,
    num_offset_1, num_offset_2,
    num_nodes, num_cells, num_types, num_static,
    val_k_gas, val_k_audio, val_k_photo, val_k_wifi, weight_types,
    num_gas_mob, p_audio_mob, p_photo_mob, p_photo_static,
    gamma_x, gamma_u, gamma_y,
    t_frame, duration,
    data_file,
    grid,
    local_util_f,
    calc_work_mat_f,
    file = "tmp/test_greedy_1_mob_300_4_4.RData"
)
