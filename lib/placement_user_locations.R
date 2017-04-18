# placement_user_locations.R
#
# Created: As part of the initial version of the project
#  Author: Charles Zhu
#
if(!exists("EX_PLACEMENT_USER_LOCATIONS_R")) {
    EX_PLACEMENT_USER_LOCATIONS_R <<- TRUE

    source("lib/basic.R")
    source("lib/square_cell_grid.R")

    library(dplyr)

# this func requires additional parameters and needs a encapsulation getter
# additional parameters: data_file, grid, num_static
# create_placement_user_locations <<- function(
#     t_frame,
#     duration,
#     val_n,
#     val_m,
#     val_k,
#     data_file,  # .RData file that contains data.frame named user_locations
#     grid,       # used to map coordinates into cell numbers
#     num_static  # number of static nodes
# ) {
#     # CHECK ARGUMENT TYPES
#     stopifnot(is.integer(t_frame))
#     stopifnot(is.integer(duration))
#     stopifnot(length(t_frame) == 1L)
#     stopifnot(length(duration) == 1L)
#     stopifnot(t_frame > 0L)
#     stopifnot(duration > 0L)
#
#     stopifnot(is.integer(val_n))
#     stopifnot(is.integer(val_m))
#     stopifnot(is.integer(val_k))
#     stopifnot(length(val_n) == 1L)
#     stopifnot(length(val_m) == 1L)
#     stopifnot(length(val_k) == 1L)
#     stopifnot(val_n > 0L)
#     stopifnot(val_m > 0L)
#     stopifnot(val_k > 0L)
#
#     stopifnot(is.character(data_file))
#     stopifnot(length(data_file) == 1L)
#
#     duration_frames = duration %/% t_frame
#     stopifnot(is.integer(duration_frames))
#
#     placement_user_locations_roll <<- array(
#         0,
#         dim = c(val_n, val_m, duration_frames + 1L)
#     )
#     dimnames(placement_user_locations_roll)[[1]] <<- z_nd_str("n", val_n)
#     dimnames(placement_user_locations_roll)[[2]] <<- z_nd_str("c", val_m)
#     dimnames(placement_user_locations_roll)[[3]] <<-
#         z_cl_str("frame", 0L:duration_frames)
#
#     stopifnot(val_n == dim(placement_user_locations_roll)[1])
#     stopifnot(val_m == dim(placement_user_locations_roll)[2])
#     stopifnot(duration_frames + 1L == dim(placement_user_locations_roll)[3])
#
#     stopifnot(is.integer(num_static))
#     stopifnot(length(num_static) == 1L)
#     stopifnot(num_static <= val_n)
#     stopifnot(num_static >= 0L)
#     num_mob = val_n - num_static
#
#     # LOAD and VALIDATE data file with grid
#     load(data_file)
#     stopifnot(exists("user_locations"))
#
#     bnd_lt = grid@num_offset_2 * grid@cell_len_x
#     bnd_tp = grid@num_offset_1 * grid@cell_len_y
#     bnd_rt = bnd_lt + grid@num_cells_2 * grid@cell_len_x
#     bnd_bm = bnd_tp + grid@num_cells_1 * grid@cell_len_y
#
#     x_min = min(user_locations[, "x"])
#     x_max = max(user_locations[, "x"])
#     y_min = min(user_locations[, "y"])
#     y_max = max(user_locations[, "y"])
#     stopifnot(x_min > bnd_lt)
#     stopifnot(x_max < bnd_rt)
#     stopifnot(y_min > bnd_tp)
#     stopifnot(y_max < bnd_bm)
#
#     user_locations_time_base = 0L
#
#     t_min = min(user_locations[, "t"])
#     t_max = max(user_locations[, "t"])
#     stopifnot(t_min >= user_locations_time_base)
#     stopifnot(t_max >= duration)
#
#     # we require trace of each node starts at time 0 in this placement model
#     user_locations_time_bases <<- rep(0L, val_n)
#     if(num_static > 0L) { # mark static nodes
#         user_locations_time_bases[(num_mob + 1L):val_n] <<- NA
#     }
#
#     # VALIDATE number of traces
#     u_ids = unique(user_locations[, "id"])
#     num_traces = length(u_ids)
#     stopifnot(num_traces >= 2L)
#     stopifnot(num_mob <= num_traces)
#     stopifnot(num_static <= nrow(user_locations))
#
#     user_locations_num_nodes <<- c(
#         num_nodes   = val_n,
#         num_mob     = num_mob,
#         num_static  = num_static
#     )
#
#     # ASSIGN random traces to nodes
#     user_locations_id_map <<- rep(NA, val_n)
#     user_locations_list_traces <<- list()
#     user_locations_static <<- matrix(NA, nrow = num_static, ncol = 2)
#     colnames(user_locations_static) <<- c("x", "y")
#
#     # pick traces for mobile nodes
#     if(num_mob > 0L) {
#         user_locations_id_map[1:num_mob] <<- sort(
#             sample(
#                 x = u_ids,
#                 size = num_mob
#             )
#         )
#         for(jnd in 1L:num_mob) {
#             user_locations_list_traces[[jnd]] <<- data.matrix(summarise(
#                 group_by(
#                     filter(
#                         user_locations,
#                         id == user_locations_id_map[jnd]
#                     ),
#                     t
#                 ),
#                 x = mean(x),
#                 y = mean(y)
#             ))
#         }
#     }
#
#     # pick locations for static nodes
#     if(num_static > 0L) {
#         user_locations_static[] <<- data.matrix(user_locations[
#             sample(
#                 x = nrow(user_locations),
#                 size = num_static
#             ),
#             c("x", "y")
#         ])
#     }
#
#     placement_user_locations_roll # RETURN
# }
# global obj generated in this func
#       placement_user_locations_roll
#       user_locations_num_nodes
#       user_locations_time_bases
#       user_locations_id_map (probably not necessary)
#       user_locations_list_traces
#       user_locations_static

# this func requires additional parameters and needs a encapsulation getter
# additional parameters: num_static
recreate_placement_user_locations <<- function(
    t_frame,
    duration,
    val_n,
    val_m,
    val_k,
    num_static  # number of static nodes
) {
    # CHECK ARGUMENT TYPES
    stopifnot(is.integer(t_frame))
    stopifnot(is.integer(duration))
    stopifnot(length(t_frame) == 1L)
    stopifnot(length(duration) == 1L)
    stopifnot(t_frame > 0L)
    stopifnot(duration > 0L)

    stopifnot(is.integer(val_n))
    stopifnot(is.integer(val_m))
    stopifnot(is.integer(val_k))
    stopifnot(length(val_n) == 1L)
    stopifnot(length(val_m) == 1L)
    stopifnot(length(val_k) == 1L)
    stopifnot(val_n > 0L)
    stopifnot(val_m > 0L)
    stopifnot(val_k > 0L)

    duration_frames = duration %/% t_frame
    stopifnot(is.integer(duration_frames))

    placement_user_locations_roll <<- array(
        0,
        dim = c(val_n, val_m, duration_frames + 1L)
    )
    dimnames(placement_user_locations_roll)[[1]] <<- z_nd_str("n", val_n)
    dimnames(placement_user_locations_roll)[[2]] <<- z_nd_str("c", val_m)
    dimnames(placement_user_locations_roll)[[3]] <<-
        z_cl_str("frame", 0L:duration_frames)

    stopifnot(val_n == dim(placement_user_locations_roll)[1])
    stopifnot(val_m == dim(placement_user_locations_roll)[2])
    stopifnot(duration_frames + 1L == dim(placement_user_locations_roll)[3])

    stopifnot(is.integer(num_static))
    stopifnot(length(num_static) == 1L)
    stopifnot(num_static <= val_n)
    stopifnot(num_static >= 0L)
    num_mob = val_n - num_static

    # VALIDATE number of traces
    num_traces = length(user_locations_list_traces)
    stopifnot(num_traces >= 2L)
    stopifnot(num_mob <= num_traces)
    stopifnot(num_static <= nrow(user_locations_static))

    user_locations_num_nodes <<- c(
        num_nodes   = val_n,
        num_mob     = num_mob,
        num_static  = num_static
    ) # UPDATE upon recreation

    placement_user_locations_roll # RETURN
}
# global obj updated in this func
#       placement_user_locations_roll
#       user_locations_num_nodes

# this func requires additional parameters and needs a encapsulation getter
# additional parameters: data_file, grid, num_static
create_placement_user_locations <<- function(
    t_frame,
    duration,
    val_n,
    val_m,
    val_k,
    data_file,  # .RData file that contains data.frame named user_locations
    grid,       # used to map coordinates into cell numbers
    num_static  # number of static nodes
) {
    stopifnot(is.character(data_file))
    stopifnot(length(data_file) == 1L)

    duration_frames = duration %/% t_frame
    stopifnot(is.integer(duration_frames))

    stopifnot(is.integer(num_static))
    stopifnot(length(num_static) == 1L)
    stopifnot(num_static <= val_n)
    stopifnot(num_static >= 0L)
    num_mob = val_n - num_static

    # LOAD and VALIDATE data file with grid
    load(data_file)
    stopifnot(exists("user_locations"))

    bnd_lt = grid@num_offset_2 * grid@cell_len_x
    bnd_tp = grid@num_offset_1 * grid@cell_len_y
    bnd_rt = bnd_lt + grid@num_cells_2 * grid@cell_len_x
    bnd_bm = bnd_tp + grid@num_cells_1 * grid@cell_len_y

    x_min = min(user_locations[, "x"])
    x_max = max(user_locations[, "x"])
    y_min = min(user_locations[, "y"])
    y_max = max(user_locations[, "y"])
    stopifnot(x_min > bnd_lt)
    stopifnot(x_max < bnd_rt)
    stopifnot(y_min > bnd_tp)
    stopifnot(y_max < bnd_bm)

    user_locations_time_base = 0L

    t_min = min(user_locations[, "t"])
    t_max = max(user_locations[, "t"])
    stopifnot(t_min >= user_locations_time_base)
    stopifnot(t_max >= duration)

    # we require trace of each node starts at time 0 in this placement model
    user_locations_time_bases <<- rep(0L, val_n)
    if(num_static > 0L) { # mark static nodes
        user_locations_time_bases[(num_mob + 1L):val_n] <<- NA
    }

    # VALIDATE number of traces
    u_ids = unique(user_locations[, "id"])
    num_traces = length(u_ids)
    stopifnot(num_traces >= 2L)
    stopifnot(num_mob <= num_traces)
    stopifnot(num_static <= nrow(user_locations))

    # user_locations_num_nodes <<- c(
    #     num_nodes   = val_n,
    #     num_mob     = num_mob,
    #     num_static  = num_static
    # )

    # ASSIGN random traces to nodes
    user_locations_id_map <<- rep(NA, val_n)
    user_locations_list_traces <<- list()
    user_locations_static <<- matrix(NA, nrow = num_static, ncol = 2)
    colnames(user_locations_static) <<- c("x", "y")

    # pick traces for mobile nodes
    if(num_mob > 0L) {
        user_locations_id_map[1:num_mob] <<- sort(
            sample(
                x = u_ids,
                size = num_mob
            )
        )
        for(jnd in 1L:num_mob) {
            user_locations_list_traces[[jnd]] <<- data.matrix(summarise(
                group_by(
                    filter(
                        user_locations,
                        id == user_locations_id_map[jnd]
                    ),
                    t
                ),
                x = mean(x),
                y = mean(y)
            ))
        }
    }

    # pick locations for static nodes
    if(num_static > 0L) {
        user_locations_static[] <<- data.matrix(user_locations[
            sample(
                x = nrow(user_locations),
                size = num_static
            ),
            c("x", "y")
            ])
    }

    recreate_placement_user_locations(
        t_frame = t_frame,
        duration = duration,
        val_n = val_n,
        val_m = val_m,
        val_k = val_k,
        num_static = num_static
    ) # RETURN
}

get_create_placement_user_locations_f <<- function(
    data_file,
    grid,
    num_static
) {
    function(...) {
        create_placement_user_locations(
            ...,
            data_file = data_file,
            grid = grid,
            num_static
        )
    } # RETURN
}

get_recreate_placement_user_locations_f <<- function(
    num_static
) {
    function(...) {
        recreate_placement_user_locations(
            ...,
            num_static
        )
    } # RETURN
}

# compute single node location using its trace in user_locations_list_traces
get_node_x_y_user_locations <<- function(jnd, simu_t) {
    if(jnd > user_locations_num_nodes["num_mob"]) { # static node
        return(
            user_locations_static[
                jnd - user_locations_num_nodes["num_mob"],
            ]
        )
    }

    # mobile node
    tr = user_locations_list_traces[[jnd]]
    t_vec = tr[, 1]
    t_vec_len = length(t_vec)
    if(simu_t <= t_vec[1]) return(tr[1, c(2L, 3L)])
    if(simu_t >= t_vec[t_vec_len]) return(tr[t_vec_len, c(2L, 3L)])

    t_ind = findInterval(simu_t, t_vec)
    t_lt = t_vec[t_ind]
    t_rt = t_vec[t_ind + 1L]
    tr_lt = tr[t_ind, c(2L, 3L)]
    tr_rt = tr[t_ind + 1L, c(2L, 3L)]
    (tr_rt - tr_lt) * (simu_t - t_lt) / (t_rt - t_lt) + tr_lt # RETURN
}

update_placement_user_locations <<- function(t_frame, simu_n) {
    val_n = dim(placement_user_locations_roll)[1]
    simu_t = simu_n * t_frame
    duration_frames = dim(placement_user_locations_roll)[3]

    stopifnot(simu_n >= 0L && simu_n <= duration_frames)

    for(jnd in 1L:val_n) {
        jnd_x_y = get_node_x_y_user_locations(jnd, simu_t)
        placement_user_locations_roll[
            jnd,
            x_y_to_cell_num(grid, jnd_x_y[1], jnd_x_y[2]),
            simu_n + 1L
        ] <<- 1
    }

    placement_user_locations_roll # RETURN
}

get_placement_user_locations <<- function() {
    placement_user_locations_roll # RETURN
}

} # ENDIF
