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

num_static = 60L
lockBinding("num_static", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1.5

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 10800L
lockBinding("t_frame", globalenv())
lockBinding("duration", globalenv())

# mode-specific simulation SETTINGS
load("prep_RData/impact_multi.RData")
lockBinding("data_type_spec_df", globalenv())

num_gas_mob = 4L
lockBinding("num_gas_mob", globalenv())

p_audio_mob = c(audio_1 = 0.6)
p_photo_mob = c(photo_1 = 0.9, photo_2 = 0.3)
p_photo_static = c(photo_1 = 0, photo_2 = 0.7)
lockBinding("p_audio_mob", globalenv())
lockBinding("p_photo_mob", globalenv())
lockBinding("p_photo_static", globalenv())

stopifnot(is.integer(num_cells))

get_t_imp_f = function(t_const) {as.integer(ceiling(t_const * 60 / t_frame))}
col_type_lt = cumsum(c(1L, val_k_gas, val_k_audio, val_k_photo))
col_type_rt = cumsum(c(val_k_gas, val_k_audio, val_k_photo, val_k_wifi))

# CREATE test case elements
capacity_mat = get_capacity_mat_multi(
    val_n           = num_nodes,
    num_static      = num_static,
    val_k_gas       = val_k_gas,
    val_k_audio     = val_k_audio,
    val_k_photo     = val_k_photo,
    val_k_wifi      = val_k_wifi,
    num_gas_mob     = num_gas_mob,
    p_audio_mob     = p_audio_mob,
    p_photo_mob     = p_photo_mob,
    p_photo_static  = p_photo_static
)
local_util_f = get_util_f_type("log_sum")

load("prep_RData/mob_300_4_1.RData")
create_placement_f = get_recreate_placement_user_locations_f(num_static)
update_placement_f = update_placement_user_locations
get_placement_f = get_placement_user_locations

source("lib/calc_work_fill_1.R")
source("lib/objective_multi.R")

calc_work_mat_f = calc_work_mat_fill_1
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
