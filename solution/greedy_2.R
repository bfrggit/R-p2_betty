# greedy_2.R
#
# Created: 2017-5-5
#  Author: Charles Zhu
#
if(!exists("EX_GREEDY_2_R")) {
    EX_GREEDY_2_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

calc_work_mat_greedy_2 <<- function(
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
    gamma_x,
    gamma_u,
    gamma_y
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
    mat_score_zero = mat_w # for dimension names, contents are disregarded
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = z_nd_str("c", val_m)
    colnames(xu_mat_zero) = z_nd_str("d", val_k)

    # adaptive data quota
    if(simu_n <= 0L) {
        greedy_2_data_history               <<- rep(0, duration_frames + 1L)
        names(greedy_2_data_history)        <<- dimnames(placement_roll)[[3]]
        greedy_2_quota_history              <<- greedy_2_data_history
        greedy_2_quota_history[simu_n + 1L] <<- data_quota
    } else {
        greedy_2_quota_history[simu_n + 1L] <<- data_quota +
            greedy_2_quota_history[simu_n] -
            greedy_2_data_history[simu_n]
    }
    this_quota = greedy_2_quota_history[simu_n + 1L]

    # initial mat x at current time frame, assuming zero mat x_0
    x_mat_prev = xu_mat_zero
    if(simu_n > 0L) {
        x_mat_prev[] = omg_x_mat(
            simu_n              = simu_n,
            x_0_frame_mat       = xu_mat_zero,
            arg_x_0_mat_history = x_0_mat_history,  # global
            arg_s_impact_mat    = s_impact_mat,     # global
            arg_t_impact_mat    = t_impact_mat      # global
        )
    }

    # initial val of obj maintained in loops
    x_mat_init = x_mat_prev
    u_mat_init = xu_mat_zero
    y_vec_init = omg_y_vec(mat_w)
    d_vec_init = omg_d_vec(data_type_specs, mat_w)
    d_val_init = sum(d_vec_init)

    omega_init = array(0, dim = c(val_m, val_n, val_k))
    dimnames(omega_init)[[1]] = z_nd_str("c", val_m)
    dimnames(omega_init)[[2]] = z_nd_str("n", val_n)
    dimnames(omega_init)[[3]] = z_nd_str("d", val_k)

    #-----------------------------------------------------------------------
    # DIFFERENCE: greedy_1 loops over nodes, while greedy_2 loops over cells

    # score mat should contain scores for each cell and each sensor type
    # need to build a similar capacity mat for cells
    # this capacity mat should contain num of sensors for each data type
    # TODO

    # score = ifelse(capacity_mat > 0, NaN, -Inf)
    # dx_mat = ifelse(capacity_mat > 0, 0, -Inf)
    # du_mat = dx_mat
    # dy_vec = ifelse(rowSums(capacity_mat) > 0, 1, +Inf)

    # loop until all sensors (in all cells) are active,
    #            maximum score is not positive, or
    #            data quota is used up
    last_added_sensor = NULL
    num_chosen = 0L
    if(verbose) cat("...", "\n")
    while(num_chosen < num_sensor) {
        # paranoid check
        # stopifnot(num_chosen == sum(mat_w))

        # update obj to reflect last added sensor
        if(!is.null(last_added_sensor)) {
            jnd = last_added_sensor[1]  # index of added node
            knd = last_added_sensor[2]  # index of adder sensor
            ind = last_added_sensor[3]  # cell index of node jnd

            # score[jnd, knd] = -Inf
            # dx_mat[jnd, knd] = -Inf
            # du_mat[jnd, knd] = -Inf

            # update coverage
            omega_init[ind, jnd, knd] = 1
            omega_mask = omega_init[, , knd, drop = FALSE]
            x_mat_mask = omg_x_mat(
                simu_n              = 0L,
                x_0_frame_mat       = omg_x_0_mat(omega_mask),
                arg_x_0_mat_history = array(0, dim = c(val_m, 1L, 1L)),
                arg_s_impact_mat    = s_impact_mat[knd],    # should be a list
                arg_t_impact_mat    = t_impact_mat[knd]     # should be a list
            ) # m by k, k = 1
            x_mat_init[, knd] = 1 - (1 - x_mat_prev[, knd]) * (1 - x_mat_mask)

            # update util, using simplified func of objective_multi
            # only util of type knd in cell ind is affected
            # u_mat_mask = local_util_f(placement_frame[, ind] * mat_w[, knd])
            u_mat_mask = local_util_f(omega_mask[ind, , 1L])
            u_mat_init[ind, knd] = u_mat_mask

            # update y
            # only activation state of node jnd is affected
            y_vec_init[jnd] = 1

            # update d
            d_val_init = d_val_init + data_type_specs$rate[knd]
        }

        # paranoid check
        # omega_init_bak = omg_omega_mat(
        #     val_m, val_n, val_k,
        #     placement_frame = placement_frame,
        #     work_mat_frame  = mat_w
        # ) # m by n by k
        # if(simu_n > 0L) {
        #     x_mat_init_bak = omg_x_mat(
        #         simu_n              = simu_n,
        #         x_0_frame_mat       = omg_x_0_mat(omega_init_bak),
        #         arg_x_0_mat_history = x_0_mat_history,
        #         arg_s_impact_mat    = s_impact_mat,
        #         arg_t_impact_mat    = t_impact_mat
        #     ) # m by k
        # } else {
        #     x_mat_init_bak = omg_x_mat(
        #         simu_n              = simu_n,
        #         x_0_frame_mat       = omg_x_0_mat(omega_init_bak),
        #         arg_x_0_mat_history = array(0, dim = c(val_m, val_k, 1L)),
        #         arg_s_impact_mat    = s_impact_mat,
        #         arg_t_impact_mat    = t_impact_mat
        #     ) # m by k
        # }
        # u_mat_init_bak = omg_u_mat(omega_init_bak)
        # stopifnot(abs(x_mat_init - x_mat_init_bak) < 1e-14)
        # stopifnot(abs(u_mat_init - u_mat_init_bak) < 1e-14)

        x_vbt_init = omg_xu_obj_type(x_mat_init)
        u_vbt_init = omg_xu_obj_type(u_mat_init)

        # paranoid verification, part 1
        # score_old = score
        # dx_mat_old = dx_mat
        # du_mat_old = du_mat

        max_score = 0
        max_c = NULL
        proc_t_acc = c(0, 0, 0)
        for(jnd in 1L:val_n) { # for each node
            # TODO

        } # ENDFOR

        # paranoid verification, part 2
        # which sensor scores have been updated
        # if(is.null(last_added_sensor)) {
        #     stopifnot(is.finite(score) == is.nan(score_old))
        #     stopifnot(is.infinite(score) == is.infinite(score_old))
        # } else {
        #     stopifnot(is.finite(score) <= is.finite(score_old))
        #     neq_ls = which((score == score_old) == FALSE, arr.ind = TRUE)
        #     stopifnot(neq_ls[, 1] == last_added_sensor[1] |
        #               neq_ls[, 2] == last_added_sensor[2])
        # }

        # paranoid verification, part 2, x alternative
        # which delta_x eval have been updated
        # stopifnot(is.finite(dx_mat) == is.finite(dx_mat_old))
        # if(is.null(last_added_sensor)) {
        #     stopifnot(is.infinite(dx_mat) == is.infinite(dx_mat_old))
        # } else {
        #     neq_ls = which((dx_mat == dx_mat_old) == FALSE, arr.ind = TRUE)
        #     stopifnot(neq_ls[, 2] == last_added_sensor[2])
        # }

        # paranoid verification, part 2, u alternative
        # which delta_u eval have been updated
        # stopifnot(is.finite(du_mat) == is.finite(du_mat_old))
        # if(is.null(last_added_sensor)) {
        #     stopifnot(is.infinite(du_mat) == is.infinite(du_mat_old))
        # } else {
        #     neq_ls = which((du_mat == du_mat_old) == FALSE, arr.ind = TRUE)
        #     stopifnot(neq_ls[, 2] == last_added_sensor[2] &
        #               placement_frame[neq_ls[, 1]] ==
        #                   placement_frame[last_added_sensor[1]])
        # }

        last_added_sensor = max_c
        # stopifnot(num_chosen == sum(mat_w))

        if(verbose && any(proc_t_acc >= 1e-2)){
            cat(
                sprintf("    Iteration sum = %d,", num_chosen),
                sprintf("proc_t = %.0f %.0f %.0f msec",
                    1000 * proc_t_acc[1],
                    1000 * proc_t_acc[2],
                    1000 * proc_t_acc[3]
                )
            )
            if(is.null(max_c)) {
                cat("\n")
                break
            }
            cat(
                sprintf(", [%d %d %d],", max_c[3], max_c[1], max_c[2]),
                sprintf("scr = %.2e\n", max_score)
            )
        } else if(is.null(max_c)) break

        # update the solution with the chosen sensor
        mat_w[max_c[1], max_c[2]] = 1
        num_chosen = num_chosen + 1L
    } # ENDWHILE

    # print(score)
    if(verbose) {
        cat(
            sprintf("    Iteration end = %d", num_chosen),
            "...", ""
        )
    }
    greedy_2_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

    mat_w # RETURN
}

get_calc_work_mat_greedy_2_f <<- function(
    gamma_x,
    gamma_u,
    gamma_y
){
    function(...) {
        calc_work_mat_greedy_2(
            ...,
            gamma_x = gamma_x,
            gamma_u = gamma_u,
            gamma_y = gamma_y
        )
    } # RETURN
}

} # ENDIF
