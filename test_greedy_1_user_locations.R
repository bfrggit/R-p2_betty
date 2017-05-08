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

num_static = as.integer(num_nodes * 0.6)
lockBinding("num_static", globalenv())

data_quota = +Inf
lockBinding("data_quota", globalenv())

gamma_x = 1
gamma_u = 1
gamma_y = -1

t_imp_threshold = 1e-4

# general simulation SETTINGS
t_frame = 60L
duration = 180L # 10800L
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

create_placement_f = get_create_placement_user_locations_f(
    data_file = data_file,
    grid = grid,
    num_static = num_static
)

update_placement_f = update_placement_user_locations
get_placement_f = get_placement_user_locations

source("solution/greedy_1.R")

calc_work_mat_f = get_calc_work_mat_greedy_1_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y
)

source("lib/objective_multi.R")

get_objective_f = get_objective_multi_f(
    gamma_x = gamma_x,
    gamma_u = gamma_u,
    gamma_y = gamma_y,
    t_imp_threshold = t_imp_threshold,
    rich_return = TRUE,
    eval_impact = FALSE
)

evaluate_impact_mat(
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
    file = "tmp/test_greedy_1_user_locations.RData"
)
