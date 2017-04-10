# objective_multi.R
#
# Author: Charles Zhu

if(!exists("EX_OBJECTIVE_MULTI_R")) {
    EX_OBJECTIVE_MULTI_R <<- TRUE

    source("lib/basic.R")

objective_zero <<- function() {
    obj_zero = data.frame(
        overall = 0,
        cover   = 0,
        util    = 0,
        traffic = 0,
        nact    = 0,
        row.names = c("zero"),
        check.names = TRUE,
        fix.empty.names = TRUE
    )

    obj_zero # RETURN
}

get_objective_zero_f <<- function() {
    function(...) {
        objective_zero() # RETURN
    } # RETURN
}

objective_multi <<- function(
    t_frame,
    simu_n,
    val_n,
    val_m,
    val_k,
    grid,
    data_type_specs,
    capacity_mat,
    get_placement_f,
    local_util_f,
    work_mat_history,
    verbose = FALSE
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    # this function must be called continuously in one simulation case
    # remove everything before starting a new case

    # accquire rolling placement and other parameters
    placement_roll = get_placement_f()
    # duration_frames = dim(placement_roll)[3] - 1
    duration_frames = dim(work_mat_history)[3] - 1

    # compute omega mat of current time frame (simu_n)
    omega_frame_mat = array(
        0,
        dim = c(val_m, val_n, val_k)
    )
    dimnames(omega_frame_mat)[[1]] = z_nd_str("c", val_m)
    dimnames(omega_frame_mat)[[2]] = z_nd_str("n", val_n)
    dimnames(omega_frame_mat)[[3]] = z_nd_str("d", val_k)

    for(jnd in 1L:val_n) { # simu_n as index should get 1L offset
        omega_frame_mat[, jnd, ] = matrix(
            placement_roll[jnd, , simu_n + 1L],
            nrow = val_m, ncol = 1, byrow = FALSE
        ) %*% matrix(
            work_mat_history[jnd, , simu_n + 1L],
            nrow = 1, ncol = val_k, byrow = TRUE
        )
    }

    #-----------------------------------------------------------------------
    # BEGIN EVALUATION of coverage

    # create x mat to keep history values for faster future calc
    if(simu_n <= 0) { # creation is done only once
        x_0_mat_history <<- array(
            0,
            dim = c(val_m, val_k, duration_frames + 1L)
        )
        dimnames(x_0_mat_history)[[1]] <<- z_nd_str("c", val_m)
        dimnames(x_0_mat_history)[[2]] <<- z_nd_str("d", val_k)
        dimnames(x_0_mat_history)[[3]] <<- z_cl_str("frame", 0L:duration_frames)
    }

    # compute x_0 mat of current time frame
    x_0_frame_mat = matrix(0, nrow = val_m, ncol = val_k)
    rownames(x_0_frame_mat) = dimnames(x_0_mat_history)[[1]]
    colnames(x_0_frame_mat) = dimnames(x_0_mat_history)[[2]]

    x_0_frame_mat[] = apply(
        omega_frame_mat,
        MARGIN = c(1, 3),
        FUN = function(omega_vec) {
            1 - prod(1 - omega_vec)
        }
    )

    # keep history of x mat
    # simu_n as index should get 1L offset
    x_0_mat_history[, , simu_n + 1L] <<- x_0_frame_mat

    # evalutate impact func for all cell/time combinations
    if(simu_n <=0) { # evalutaion is done only once
        grid_distance_mat <<- cell_dist_mat(grid)
        time_distance_vec <<- duration_frames:0L
        s_impact_mat_zero <<- matrix(0, nrow = val_m, ncol = val_m)
        rownames(s_impact_mat_zero) <<- z_nd_str("c", val_m)
        colnames(s_impact_mat_zero) <<- z_nd_str("c", val_m)
        s_impact_mat <<- rep(list(s_impact_mat_zero), val_k)
        t_impact_mat <<- rep(list(rep(0, duration_frames + 1L)), val_k)

        for(knd in 1:val_k) {
            s_impact_mat[[knd]][] <<- do.call(
                data_type_specs[knd, "s_impact_f"],
                list(grid_distance_mat)
            )
            t_impact_mat[[knd]] <<- do.call(
                data_type_specs[knd, "t_impact_f"],
                list(time_distance_vec)
            )
        }
    }

    # compute x mat of current time frame
    x_frame_mat = matrix(0, nrow = val_m, ncol = val_k)
    rownames(x_0_frame_mat) = dimnames(x_0_mat_history)[[1]]
    colnames(x_0_frame_mat) = dimnames(x_0_mat_history)[[2]]

    for(ind in 1L:val_m) {
        for(knd in 1L:val_k) {
            s_imp = s_impact_mat[[knd]]
            t_imp = t_impact_mat[[knd]]

            # TEST of computation speed using ALL-ZERO matrices
            # s_imp = matrix(0, nrow = val_m, ncol = val_m)
            # t_imp = rep(0, duration_frames + 1L)

            # TEST of computation speed using ALL-ONE mattrices
            # s_imp = matrix(1, nrow = val_m, ncol = val_m)
            # t_imp = rep(1, duration_frames + 1L)

            st_imp_ind_knd = matrix(
                s_imp[ind, ],
                nrow = val_m, ncol = 1, byrow = FALSE
            ) %*% matrix(
                t_imp[(duration_frames - simu_n + 1L):(duration_frames + 1L)],
                nrow = 1, ncol = simu_n + 1L, byrow = FALSE
            )
            x_frame_mat[ind, knd] = 1 - prod(
                1 - x_0_mat_history[, knd, 1L:(simu_n + 1L)] * st_imp_ind_knd
            )
        }
    }

    # compute x_objective
    x_objective_frame = sum(x_frame_mat) / val_m / val_k

    #-----------------------------------------------------------------------
    # END EVALUATION of coverage

    # compute u mat of current time frame
    u_frame_mat = matrix(0, nrow = val_m, ncol = val_k)
    rownames(u_frame_mat) = dimnames(x_0_mat_history)[[1]]
    colnames(u_frame_mat) = dimnames(x_0_mat_history)[[2]]

    u_frame_mat[] = apply(
        omega_frame_mat,
        MARGIN = c(1, 3),
        FUN = function(omega_vec) {
            local_util_f(omega_vec)
        }
    )

    # compute u objective
    u_objective_frame = sum(u_frame_mat) / val_m / val_k

    # construct result
    obj_res = objective_zero()
    obj_res["cover"] = x_objective_frame
    obj_res["util"] = u_objective_frame
    obj_res # RETURN
}

get_objective_multi_f <<- function() {
    function(...) {
        objective_multi(...) # RETURN
    } # RETURN
}

} # ENDIF
