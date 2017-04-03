# PRIMARY SIMULATION function
# handle loops for a complete simulation
simulate_beta = function(
    t_frame,                # time frame length, sec
    duration,               # total length of simulation in simulated time, sec
    val_n,
    val_m,
    val_k,
    grid,                   # grid specs
    data_type_specs,        # data type specs data frame
    capacity_mat,           # sensing capacity matrix
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
    
    stopifnot(is.function(update_placement_f))
    stopifnot(is.function(get_placement_f))
    stopifnot(is.function(calc_work_mat_f))
    
    stopifnot(is.logical(verbose))
    
    for(simu_t in seq(0L, duration, by = t_frame)) {
        if(verbose) cat("Simulation time =", simu_t, "\n")
        simu_n = as.integer(simu_t %/% t_frame)
        update_placement_f()
        # placement_mat = get_palcement_f()
        work_mat = calc_work_mat_f(
            t_frame,
            simu_t,
            val_n,
            val_m,
            val_k,
            grid,
            data_type_specs,
            capacity_mat,
            get_placement_f, # FIXME: not implemented
            verbose = verbose
        ) # FIXME: not implemented
        # TODO: calculate objective function values and keep track of them
    }
}