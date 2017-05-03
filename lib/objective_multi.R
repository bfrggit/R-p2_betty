# objective_multi.R
#
# Created: As part of the initial version of the project
# Updated: 2017-5-2
#  Author: Charles Zhu
#
if(!exists("EX_OBJECTIVE_MULTI_R")) {
    EX_OBJECTIVE_MULTI_R <<- TRUE

    source("lib/basic.R")

objective_zero <<- function() {
    c(
        overall = 0,
        cover   = 0,
        util    = 0,
        traffic = 0,
        nact    = 0
    ) # RETURN
}

get_objective_zero_f <<- function() {
    function(...) {
        objective_zero() # RETURN
    } # RETURN
}

# func module called by main func
# user can opt to call it independently
evaluate_impact_mat <<- function(
    duration_frames,        # index of last time frame counting from 0
    val_m,
    val_k,
    grid,                   # grid specs
    data_type_specs         # data type specs data frame
) {
    stopifnot(val_m == grid@num_cells_1 * grid@num_cells_2)
    stopifnot(val_k == nrow(data_type_specs))

    grid_distance_mat <<- cell_dist_mat(grid)
    time_distance_vec <<- duration_frames:0L
    s_impact_mat_zero <<- matrix(0, nrow = val_m, ncol = val_m)
    rownames(s_impact_mat_zero) <<- z_nd_str("c", val_m)
    colnames(s_impact_mat_zero) <<- z_nd_str("c", val_m)
    s_impact_mat <<- rep(list(s_impact_mat_zero), val_k)
    t_impact_mat <<- rep(list(1), val_k)

    for(knd in 1L:val_k) {
        s_impact_mat[[knd]][] <<- do.call(
            data_type_specs[knd, "s_impact_f"],
            list(grid_distance_mat)
        )
        t_impact_vec = do.call(
            data_type_specs[knd, "t_impact_f"],
            list(time_distance_vec)
        )
        if(t_impact_vec[1] < t_imp_threshold) {
            t_impact_vec = t_impact_vec[
                (findInterval(
                    t_imp_threshold,
                    t_impact_vec
                ) + 1L):length(t_impact_vec)
                ]
        }
        t_impact_mat[[knd]] <<- t_impact_vec
    }

    NA # NO RETURN
}
# global obj created in this func
#       grid_distance_mat
#       time_distance_vec
#       s_impact_mat_zero
#       s_impact_mat
#       t_impact_mat

# func module called by main func
# could be useful in solution func, too
omg_omega_mat <<- function(
    val_m,
    val_n,
    val_k,
    placement_frame,        # placement mat at current time frame
    work_mat_frame          # work mat at current time frame
) {
    omega_frame_mat = array(0, dim = c(val_m, val_n, val_k))
    dimnames(omega_frame_mat)[[1]] = z_nd_str("c", val_m)
    dimnames(omega_frame_mat)[[2]] = z_nd_str("n", val_n)
    dimnames(omega_frame_mat)[[3]] = z_nd_str("d", val_k)

    for(jnd in 1L:val_n) {
        omega_frame_mat[, jnd, ] =
            placement_frame[jnd, ] %*%
            work_mat_frame[jnd, , drop = FALSE]
    }

    omega_frame_mat # RETURN
}

#---------------------------------------------------------------------------
# BEGIN EVALUATION functions of coverage objective

omg_x_0_mat <<- function(omega_frame_mat) {
    # val_m = dim(omega_frame_mat)[1]
    # val_k = dim(omega_frame_mat)[3]
    # x_0_frame_mat = matrix(0, nrow = val_m, ncol = val_k)
    # rownames(x_0_frame_mat) = dimnames(omega_frame_mat)[[1]]
    # colnames(x_0_frame_mat) = dimnames(omega_frame_mat)[[3]]

    x_0_frame_mat = apply(
        omega_frame_mat,
        MARGIN = c(1, 3),
        FUN = function(omega_vec) {
            1 - prod(1 - omega_vec)
        }
    ) # RETURN
}

omg_x_mat <<- function(
    simu_n,
    x_0_frame_mat,
    arg_x_0_mat_history,
    arg_s_impact_mat,
    arg_t_impact_mat
) {
    # val_m = nrow(x_0_frame_mat)
    # val_k = ncol(x_0_frame_mat)
    # x_frame_mat = matrix(0, nrow = val_m, ncol = val_k)
    # rownames(x_frame_mat) = rownames(x_0_frame_mat)
    # colnames(x_frame_mat) = colnames(x_0_frame_mat)

    x_frame_mat = x_0_frame_mat # for dimension names, contents are disregarded
    val_m = nrow(x_frame_mat)
    val_k = ncol(x_frame_mat)
    x_0_mat_combo = arg_x_0_mat_history
    x_0_mat_combo[, , simu_n + 1L] = x_0_frame_mat

    for(ind in 1L:val_m) {
        for(knd in 1L:val_k) {
            s_imp = arg_s_impact_mat[[knd]] # mat, m by m
            t_imp = arg_t_impact_mat[[knd]] # vec
            t_len_all = length(t_imp)
            t_imp_len = min(simu_n + 1L, length(t_imp))

            # create spatial-temporal impact matrix
            st_imp_ind_knd = s_imp[ind, ] %*%
                t(t_imp[(t_len_all - t_imp_len + 1L):t_len_all])

            # compute probability product
            x_frame_mat[ind, knd] = 1 - prod(
                1 - x_0_mat_combo[
                    ,
                    knd,
                    (simu_n - t_imp_len + 2L):(simu_n + 1L)
                ] * st_imp_ind_knd
            )
        }
    }

    x_frame_mat # RETURN
}

omg_xu_obj_type <<- function(xu_frame_mat) {
    xu_frame_by_type = apply(xu_frame_mat, MARGIN = 2, FUN = mean) # RETURN
}

omg_xu_obj <<- function(data_type_specs, xu_frame_by_type) {
    xu_objective_frame = sum(
        xu_frame_by_type * data_type_specs$weight
    ) / sum(data_type_specs$weight) # RETURN
}

#---------------------------------------------------------------------------
# END EVALUATION functions of coverage objective

omg_u_mat <<- function(omega_frame_mat) {
    u_frame_mat = apply(
        omega_frame_mat,
        MARGIN = c(1, 3),
        FUN = local_util_f
    ) # RETURN
}

omg_y_vec <<- function(work_mat_frame) {
    y_frame_vec = apply(
        work_mat_frame,
        MARGIN = 1,
        FUN = function(node_vec) {
            1 - prod(1 - node_vec)
        }
    ) # RETURN
}

omg_d_vec <<- function(data_type_specs, work_mat_frame) {
    colSums(work_mat_frame) * data_type_specs$rate # RETURN
}

# MAIN objective func used in simulations
objective_multi <<- function(
    t_frame,                # time frame length, sec
    simu_n,                 # index of current time frame counting from 0
    val_n,
    val_m,
    val_k,
    grid,                   # grid specs
    data_type_specs,        # data type specs data frame
    capacity_mat,           # sensing capacity matrix (should not need)
    get_placement_f,        # function to get placement matrix
    local_util_f,           # function to evaluate local util in each cell
    work_mat_history,       # history of work_mat maintained by simulation
    verbose = FALSE,
    gamma_x,                # weight of coverage in overall obj
    gamma_u,                # weight of util in overall obj
    gamma_y,                # weight of number of active nodes in overall obj
    t_imp_threshold,        # minimum temporal impact to consider in total
    rich_return,            # whether or not to return a list of multiple obj
    eval_impact             # whether or not to evaluate impact mat
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    # this function must be called continuously in one simulation case
    # remove everything before starting a new case

    # acquire rolling placement and other parameters
    placement_roll = get_placement_f()
    # duration_frames = dim(placement_roll)[3] - 1
    duration_frames = dim(work_mat_history)[3] - 1 # should be the same

    # compute omega mat of current time frame (simu_n)
    omega_frame_mat = omg_omega_mat(
        val_m           = val_m,
        val_n           = val_n,
        val_k           = val_k,
        placement_frame = placement_roll[, , simu_n + 1L],
        work_mat_frame  = work_mat_history[, , simu_n + 1L]
    )

    # create x mat to keep history values for faster future calc
    if(simu_n <= 0L) { # creation is done only once
        x_0_mat_history <<- array(
            0,
            dim = c(val_m, val_k, duration_frames + 1L)
        )
        dimnames(x_0_mat_history)[[1]] <<- z_nd_str("c", val_m)
        dimnames(x_0_mat_history)[[2]] <<- z_nd_str("d", val_k)
        dimnames(x_0_mat_history)[[3]] <<- z_cl_str("frame", 0L:duration_frames)
    }

    # evaluate impact func for all cell/time combinations
    if(eval_impact && simu_n <= 0L) { # evaluation is done only once
        evaluate_impact_mat(
            duration_frames = duration_frames,
            val_m           = val_m,
            val_k           = val_k,
            grid            = grid,
            data_type_specs = data_type_specs
        )
    }

    # EVALUATION of coverage
    x_0_frame_mat = omg_x_0_mat(omega_frame_mat)
    x_frame_mat = omg_x_mat(
        simu_n              = simu_n,
        x_0_frame_mat       = x_0_frame_mat,
        arg_x_0_mat_history = x_0_mat_history,  # global
        arg_s_impact_mat    = s_impact_mat,     # global
        arg_t_impact_mat    = t_impact_mat      # global
    )
    x_objective_frame_by_type = omg_xu_obj_type(x_frame_mat)
    x_objective_frame = omg_xu_obj(data_type_specs, x_objective_frame_by_type)

    # EVALUATION of utility
    u_frame_mat = omg_u_mat(omega_frame_mat)
    u_objective_frame_by_type = omg_xu_obj_type(u_frame_mat)
    u_objective_frame = omg_xu_obj(data_type_specs, u_objective_frame_by_type)

    # EVALUATION of num of active nodes
    y_frame_vec = omg_y_vec(work_mat_history[, , simu_n + 1L])
    y_objective_frame = sum(y_frame_vec) / val_m / val_k

    # EVALUATION of traffic
    d_frame_vec = omg_d_vec(data_type_specs, work_mat_history[, , simu_n + 1L])
    d_objective_frame = sum(d_frame_vec)

    # keep history of x mat
    # simu_n as index should get 1L offset
    x_0_mat_history[, , simu_n + 1L] <<- x_0_frame_mat

    # construct result
    obj_res = objective_zero()
    obj_res["cover"]    = x_objective_frame
    obj_res["util"]     = u_objective_frame
    obj_res["traffic"]  = d_objective_frame
    obj_res["nact"]     = y_objective_frame
    obj_res["overall"]  = gamma_x * x_objective_frame +
                          gamma_u * u_objective_frame +
                          gamma_y * y_objective_frame

    # general RETURN
    if(!rich_return) {
        return(obj_res) # RETURN
    }

    # rich RETURN
    list(
        general = obj_res,
        x_by_type = x_objective_frame_by_type,
        u_by_type = u_objective_frame_by_type
    ) # RETURN
}

# wrapper for general objective func interface
get_objective_multi_f <<- function(
    gamma_x,
    gamma_u,
    gamma_y,
    t_imp_threshold = 0,
    rich_return = FALSE,
    eval_impact = TRUE
) {
    stopifnot(is.numeric(gamma_x))
    stopifnot(is.numeric(gamma_u))
    stopifnot(is.numeric(gamma_y))

    stopifnot(is.numeric(t_imp_threshold))
    stopifnot(t_imp_threshold >= 0)
    stopifnot(t_imp_threshold <= 1)

    stopifnot(is.logical(rich_return))
    stopifnot(length(rich_return) == 1L)

    function(...) {
        objective_multi(
            ...,
            gamma_x = gamma_x,
            gamma_u = gamma_u,
            gamma_y = gamma_y,
            t_imp_threshold = t_imp_threshold,
            rich_return = rich_return,
            eval_impact = eval_impact
        ) # RETURN
    } # RETURN
}

} # ENDIF
