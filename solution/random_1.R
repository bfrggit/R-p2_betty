# random_1.R
#
# Created: 2017-7-28
#  Author: Charles Zhu
#
if(!exists("EX_RANDOM_1_R")) {
    EX_RANDOM_1_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

calc_work_mat_random_1 <<- function(
    t_frame,                # time frame length, sec
    simu_n,                 # time frame number, on which simulation is running
    val_n,
    val_m,
    val_k,
    grid,                   # unused in this example implementation
    data_type_specs,
    capacity_mat,
    get_placement_f,
    local_util_f,           # unused in this example implementation
    data_quota,
    verbose = FALSE,
    seed = NULL
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    mat_w = matrix(
        0,
        nrow = val_n,
        ncol = val_k
    )
    rownames(mat_w) = z_nd_str("n", val_n)
    colnames(mat_w) = z_nd_str("d", val_k)

    # override verbose msg
    # verbose = FALSE

    # prepare data for quick access
    num_sensor = sum(capacity_mat)
    placement_roll = get_placement_f()
    placement_frame = placement_roll[, , simu_n + 1L]
    duration_frames = dim(placement_roll)[3] - 1L
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = z_nd_str("c", val_m)
    colnames(xu_mat_zero) = z_nd_str("d", val_k)

    # adaptive data quota
    if(simu_n <= 0L) {
        random_1_data_history               <<- rep(0, duration_frames + 1L)
        names(random_1_data_history)        <<- dimnames(placement_roll)[[3]]
        random_1_quota_history              <<- random_1_data_history
        random_1_quota_history[simu_n + 1L] <<- data_quota
    } else {
        random_1_quota_history[simu_n + 1L] <<- data_quota +
            random_1_quota_history[simu_n] -
            random_1_data_history[simu_n]
    }
    this_quota = random_1_quota_history[simu_n + 1L]

    # initial val of obj maintained in loops
    y_vec_init = omg_y_vec(mat_w)
    d_vec_init = omg_d_vec(data_type_specs, mat_w)
    d_val_init = sum(d_vec_init)

    # compute best possible omega and number of sensor of each type in each cell
    omega_all = omg_omega_mat(
        val_m = val_m, val_n = val_n, val_k = val_k,
        placement_frame = placement_frame,
        work_mat_frame = capacity_mat
    )
    max_sensor_each_cell = apply(omega_all, MARGIN = c(1, 3), FUN = sum)
    stopifnot(sum(max_sensor_each_cell) == num_sensor)
    sensor_each_cell = max_sensor_each_cell
    sensor_each_cell[] = 0

    #-----------------------------------------------------------------------
    # all lines above are preparation for context

    # loop until all sensors (on all nodes) are active,
    #            maximum score is not positive, or
    #            data quota is used up
    last_added_sensor = NULL
    num_chosen = 0L
    set.seed(seed)
    while(num_chosen < num_sensor) {
        # if number of active sensor in a cell is equal to its maximum,
        # this cell-type pair is full
        # select all cell-type pairs that are not full as valid
        # mark invalid pairs as Inf so they are never selected
        ls_sensor_ec_vali = which(max_sensor_each_cell <= sensor_each_cell)
        sensor_vali = sensor_each_cell
        sensor_vali[ls_sensor_ec_vali] = Inf

        # select sensor types whose data generation rate is smaller than
        # remaining data quota for this time frame
        # filter data structures to keep those types only
        type_enough_d = which(data_type_specs$rate <= this_quota - d_val_init)
        if(length(type_enough_d) < 1L) break
        sensor_cand_enough_d = sensor_vali[, type_enough_d]

        # for all valid pairs in valid types
        # find minimum number of active sensors
        s_cand_ed_min = min(sensor_cand_enough_d)
        if(!is.finite(s_cand_ed_min)) break

        # all valid pairs that have this minimum number of active sensors
        # are seen as candidates for random selection
        sensor_cand = which(sensor_cand_enough_d == s_cand_ed_min, arr.ind = T)

        # if any sensor can be activated to cover its type for a cell
        if(length(sensor_cand) > 0L) {
            sensor_rnd = sensor_cand[sample(nrow(sensor_cand), size = 1L), ]
            inr = sensor_rnd[1]
            knr = type_enough_d[sensor_rnd[2]]

            # pick actual node in corresponding cell
            node_cand = which(capacity_mat[, knr] > 0 & mat_w[, knr] < 1 &
                y_vec_init > 0)
            if(length(node_cand) < 1L) {
                node_cand = which(capacity_mat[, knr] > 0 & mat_w[, knr] < 1)
            }
            stopifnot(length(node_cand) > 0L)
            if(length(node_cand) > 1L) {
                jnr = sample(node_cand, size = 1L)
            } else jnr = node_cand
            # stopifnot(capacity_mat[jnr, knr] > 0)

            # update data for next loop
            mat_w[jnr, knr] = 1
            y_vec_init[jnr] = 1
            d_val_init = d_val_init + data_type_specs$rate[knr]
            sensor_each_cell[inr, knr] = sensor_each_cell[inr, knr] + 1
            num_chosen = num_chosen + 1L
        } else break
    } # ENDWHILE

    random_1_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

    mat_w # RETURN
}

get_calc_work_mat_random_1_f <<- function(seed = NULL){
    function(...) {
        calc_work_mat_random_1(..., seed)
    } # RETURN
}

} # ENDIF
