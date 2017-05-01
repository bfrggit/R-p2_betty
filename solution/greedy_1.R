# greedy_1.R
#
# Created: 2017-4-30
#  Coding: Charles Zhu
#
if(!exists("EX_GREEDY_1_R")) {
    EX_GREEDY_1_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

calc_work_mat_greedy_1 <<- function(
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

    # prepare data for quick access
    num_sensor = sum(capacity_mat)
    placement_frame = get_placement_f()[, , simu_n + 1L]
    mat_score_zero = mat_w # for dimension names, contents are disregarded
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = z_nd_str("c", val_m)
    colnames(xu_mat_zero) = z_nd_str("d", val_k)

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

    # loop until all sensors (on all nodes) are active,
    #            maximum score is not positive, or
    #            data quota is used up
    last_added_sensor = NULL
    num_chosen = 0L
    if(verbose) cat("...", "\n")
    while(num_chosen < num_sensor) {
        # paranoid check
        # stopifnot(num_chosen == sum(mat_w))

        # obtain original coverage x, assuming zero work mat
        if(is.null(last_added_sensor)) { # first time to search
            x_mat_init = x_mat_prev
            u_mat_init = xu_mat_zero
            y_vec_init = omg_y_vec(mat_w)
            d_vec_init = omg_d_vec(data_type_specs, mat_w)
            d_val_init = sum(d_vec_init)
        } else { # update obj to reflect last added sensor
            jnd = last_added_sensor[1]                  # index of added node
            knd = last_added_sensor[2]                  # index of adder sensor
            ind = which(placement_frame[jnd, ] == 1)[1] # cell index of node jnd

            # update coverage
            # only coverage of type knd is affected
            omega_mask = omg_omega_mat(
                val_m           = val_m,
                val_n           = val_n,
                val_k           = 1L,
                placement_frame = placement_frame,
                work_mat_frame  = t(t(mat_w[, knd]))
            ) # m by n by k, k = 1
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
            u_mat_mask = local_util_f(placement_frame[, ind] * mat_w[, knd])
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

        max_score = 0
        max_c = NULL
        proc_t_acc = c(0, 0, 0)
        for(jnd in 1L:val_n) { # for each node
            # attempt to switch on all sensors on-board
            tmp_w = mat_w
            tmp_w[jnd, ] = capacity_mat[jnd, ]

            # get candidate of sensors to activate on node jnd
            # active sensors in mat_w cannot be activated again
            tmp_knd_cand = which(tmp_w[jnd, ] - mat_w[jnd, ] > 0)
            if(length(tmp_knd_cand) <= 0L) next

            # compute obj
            tmp_omega = omg_omega_mat(
                val_m, val_n, val_k,
                placement_frame, tmp_w
            )
            proc_t = proc.time()[3]
            tmp_x_0_mat = omg_x_0_mat(tmp_omega) # slow step
            proc_t_acc[1] = proc_t_acc[1] + proc.time()[3] - proc_t
            proc_t = proc.time()[3]
            tmp_x_cur = omg_x_mat(
                simu_n              = 0L,
                x_0_frame_mat       = tmp_x_0_mat,
                arg_x_0_mat_history = array(0, dim = c(val_m, val_k, 1L)),
                arg_s_impact_mat    = s_impact_mat,     # global
                arg_t_impact_mat    = t_impact_mat      # global
            ) # slow step
            proc_t_acc[2] = proc_t_acc[2] + proc.time()[3] - proc_t
            tmp_x_mat = 1 - (1 - x_mat_prev) * (1 - tmp_x_cur)
            tmp_x_vbt = omg_xu_obj_type(tmp_x_mat)
            proc_t = proc.time()[3]
            tmp_u_mat = omg_u_mat(tmp_omega) # slow step
            proc_t_acc[3] = proc_t_acc[3] + proc.time()[3] - proc_t
            tmp_u_vbt = omg_xu_obj_type(tmp_u_mat)

            # compute delta obj
            delta_x_t = tmp_x_vbt - x_vbt_init
            delta_u_t = tmp_u_vbt - u_vbt_init
            delta_y = (1 - y_vec_init[jnd]) / val_m / val_k *
                (tmp_w[jnd, ] - mat_w[jnd, ])
            delta_d_t = data_type_specs$rate

            # compute score
            score_vec = ((gamma_x * delta_x_t + gamma_u * delta_u_t) *
                data_type_specs$weight +
                gamma_y * delta_y) / delta_d_t
            for(knd in tmp_knd_cand) {
                if(score_vec[knd] > max_score) {
                    max_score = score_vec[knd]
                    max_c = c(jnd = jnd, knd = knd)
                }
            }
        }
        if(is.null(max_c)) break
        if(d_val_init + data_type_specs[max_c[2], "rate"] > data_quota) break
        mat_w[max_c[1], max_c[2]] = 1
        if(verbose) cat(
            sprintf("    Iteration sum = %d,", sum(mat_w)),
            sprintf("[%d,", which(placement_frame[max_c[1], ] == 1)[1]),
            sprintf("%d,", max_c[1]),
            sprintf("%d],", max_c[2]),
            sprintf("scr = %.2e,", max_score),
            sprintf("proc_t = %.0f %.0f %.0f msec\n",
                1000 * proc_t_acc[1],
                1000 * proc_t_acc[2],
                1000 * proc_t_acc[3]
            )
        )
        last_added_sensor = max_c
        num_chosen = num_chosen + 1L
    }
    if(verbose) cat("...", "")

    mat_w # RETURN
}

get_calc_work_mat_greedy_1_f <<- function(
    gamma_x,
    gamma_u,
    gamma_y
){
    function(...) {
        calc_work_mat_greedy_1(
            ...,
            gamma_x = gamma_x,
            gamma_u = gamma_u,
            gamma_y = gamma_y
        )
    } # RETURN
}

} # ENDIF
