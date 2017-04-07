# objective_multi.R
#
# Author: Charles Zhu

if(!exists("EX_OBJECTIVE_MULTI_R")) {
    EX_OBJECTIVE_MULTI_R <<- TRUE

    source("lib/basic.R")

objective_zero <<- function() {
    obj_zero = data.frame(
        0,
        0,
        0,
        0,
        0,
        row.names = c("zero"),
        check.names = TRUE,
        fix.empty.names = TRUE
    )
    colnames(obj_zero) = c("overall", "cover", "util", "traffic", "nact")

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
    work_mat_history,
    verbose = FALSE
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

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

    for(jnd in 1L:val_n) {
        omega_frame_mat[, jnd, ] = matrix(
            placement_roll[jnd, , simu_n],
            nrow = val_m, ncol = 1, byrow = FALSE
        ) %*% matrix(
            work_mat_history[jnd, , simu_n],
            nrow = 1, ncol = val_k, byrow = TRUE
        )
    }

    # create x mat to keep history values for faster future calc
    if(simu_n <= 0) {
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
    x_0_mat_history[, , simu_n] <<- x_0_frame_mat

    # evalutate impact func
    # TODO

    objective_zero()
}

get_objective_multi_f <<- function() {
    function(...) {
        objective_multi(...) # RETURN
    } # RETURN
}

} # ENDIF
