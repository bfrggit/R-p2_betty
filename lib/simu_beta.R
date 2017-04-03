# simu_beta.R
#
# Author: Charles Zhu

if(!exists("EX_SIMU_BETA_R")) {
    EX_SIMU_BETA_R <<- TRUE

# PRIMARY SIMULATION function
# handle loops for a complete simulation
simulate_beta <<- function(
    t_frame,                # time frame length, sec
    duration,               # total length of simulation in simulated time, sec
    val_n,
    val_m,
    val_k,
    grid,                   # grid specs
    data_type_specs,        # data type specs data frame
    capacity_mat,           # sensing capacity matrix
    create_placement_f,     # function to create node placement or load file
    update_placement_f,     # function to update node placement
    get_placement_f,        # function to get placement matrix
    calc_work_mat_f,        # PRIMARY function that implements an ALGORITHM
    verbose = FALSE
) {
    # CHECK ARGUMENT TYPES
    stopifnot(is.integer(t_frame))
    stopifnot(is.integer(duration))
    stopifnot(t_frame > 0)
    stopifnot(duration > 0)

    stopifnot(is.integer(val_n))
    stopifnot(is.integer(val_m))
    stopifnot(is.integer(val_k))
    stopifnot(val_n > 0)
    stopifnot(val_m > 0)
    stopifnot(val_k > 0)

    stopifnot(is.data.frame(data_type_specs))
    stopifnot(nrow(data_type_specs) == val_k)

    stopifnot(is.matrix(capacity_mat))
    stopifnot(nrow(capacity_mat) == val_n)
    stopifnot(ncol(capacity_mat) == val_k)

    stopifnot(is.function(create_placement_f))
    stopifnot(is.function(update_placement_f))
    stopifnot(is.function(get_placement_f))
    stopifnot(is.function(calc_work_mat_f))

    stopifnot(is.logical(verbose))

    # CREATE placement objects and INIT them if necessary
    create_placement_f(
        t_frame = t_frame,
        duration = duration,
        val_n = val_n,
        val_m = val_m,
        val_k = val_k
    )

    # PREPARE variables for operation history
    duration_frames = duration %/% t_frame
    stopifnot(is.integer(duration_frames))

    work_mat_history = array(0, dim = c(val_n, val_k, duration_frames))

    # MAIN LOOP
    for(simu_n in 0L:duration_frames) {
        simu_t = simu_n * t_frame
        if(verbose) {
            cat(
                "Simulation",
                sprintf("frame = %d of %d,", simu_n, duration_frames),
                sprintf("time = %d", simu_t),
                "\n"
            )
        }
        update_placement_f(
            t_frame = t_frame,
            simu_n = simu_n
        )
        work_mat = calc_work_mat_f(
            t_frame             = t_frame,
            simu_n              = simu_n,
            val_n               = val_n,
            val_m               = val_m,
            val_k               = val_k,
            grid                = grid,
            data_type_specs     = data_type_specs,
            capacity_mat        = capacity_mat,
            get_placement_f     = get_placement_f,
            verbose             = verbose
        ) # FIXME: not implemented

        # TODO: calculate objective function values and keep track of them
    }
}

} # ENDIF
