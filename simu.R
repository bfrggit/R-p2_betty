# PRIMARY SIMULATION function
# handle loops for a complete simulation
simulate_beta = function(
    t_frame,                # time frame length, sec
    duration,               # total length of simulation in simulated time, sec
    val_n,
    val_m,
    val_k,
    grid,                   # grid specs
    capacity_mat,           # sensing capacity matrix
    data_type_specs,        # data type specs data frame
    update_placement_f,     # function to update node placement
    get_placement_f,        # function to get placement matrix
    calc_work_mat_f,        # PRIMARY function that implements an ALGORITHM
) {
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
}