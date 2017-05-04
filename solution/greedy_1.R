# greedy_1.R
#
# Created: 2017-4-30
# Updated: 2017-5-2
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
    placement_roll = get_placement_f()
    placement_frame = placement_roll[, , simu_n + 1L]
    duration_frames = dim(placement_roll)[3] - 1L
    mat_score_zero = mat_w # for dimension names, contents are disregarded
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = z_nd_str("c", val_m)
    colnames(xu_mat_zero) = z_nd_str("d", val_k)

    # adaptive data quota
    if(simu_n <= 0L) {
        greedy_1_data_history               <<- rep(0, duration_frames + 1L)
        names(greedy_1_data_history)        <<- dimnames(placement_roll)[[3]]
        greedy_1_quota_history              <<- greedy_1_data_history
        greedy_1_quota_history[simu_n + 1L] <<- data_quota
    } else {
        greedy_1_quota_history[simu_n + 1L] <<- data_quota +
            greedy_1_quota_history[simu_n] -
            greedy_1_data_history[simu_n]
    }
    this_quota = greedy_1_quota_history[simu_n + 1L]

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

    score = ifelse(capacity_mat > 0, NaN, -Inf)
    dx_mat = ifelse(capacity_mat > 0, 0, -Inf)
    du_mat = dx_mat
    # dy_vec = ifelse(rowSums(capacity_mat) > 0, 1, +Inf)

    # loop until all sensors (on all nodes) are active,
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

            score[jnd, knd] = -Inf
            dx_mat[jnd, knd] = -Inf
            du_mat[jnd, knd] = -Inf

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
        dx_mat_old = dx_mat
        du_mat_old = du_mat

        max_score = 0
        max_c = NULL
        proc_t_acc = c(0, 0, 0)
        for(jnd in 1L:val_n) { # for each node
            # attempt to switch on all sensors on-board
            tmp_w = mat_w
            tmp_w[jnd, ] = capacity_mat[jnd, ]

            # get list of valid sensors to compare scores
            # active sensors in mat_w are not valid
            # only valid sensors can be activated
            knd_vali = which(is.finite(score[jnd, ]) | is.nan(score[jnd, ]))
            num_vali = length(knd_vali)
            if(num_vali <= 0L) next

            # paranoid check
            # knd_vali_bak = which(tmp_w[jnd, ] - mat_w[jnd, ] > 0)
            # stopifnot(knd_vali == knd_vali_bak)

            # get candidate of sensors that need their scores updated
            # candidates must be valid
            # knd_cand = knd_vali
            if(is.null(last_added_sensor) || jnd == last_added_sensor[1]) {
                knd_cand = knd_vali
            } else {
                knd_cand = last_added_sensor[2]
                if(!is.finite(score[jnd, knd_cand])) knd_cand = integer(0)
            }
            num_cand = length(knd_cand)
            # if(num_cand <= 0L) next

            if(num_cand > 0L && is.null(last_added_sensor)){ # first selection
                # compute obj
                tmp_omega = omg_omega_mat(
                    val_m, val_n, num_cand,
                    placement_frame, tmp_w[, knd_cand, drop = FALSE]
                )
                proc_t = proc.time()[3]
                tmp_x_0_mat = omg_x_0_mat(tmp_omega) # slow step
                proc_t_acc[1] = proc_t_acc[1] + proc.time()[3] - proc_t
                proc_t = proc.time()[3]
                tmp_x_cur = omg_x_mat(
                    simu_n              = 0L,
                    x_0_frame_mat       = tmp_x_0_mat,
                    arg_x_0_mat_history = array(0,
                        dim = c(val_m, num_cand, 1L)),
                    arg_s_impact_mat    = s_impact_mat[knd_cand],   # global
                    arg_t_impact_mat    = t_impact_mat[knd_cand]    # global
                ) # slow step
                proc_t_acc[2] = proc_t_acc[2] + proc.time()[3] - proc_t
                tmp_x_mat = 1 - (1 - x_mat_prev[, knd_cand]) * (1 - tmp_x_cur)
                tmp_x_vbt = omg_xu_obj_type(tmp_x_mat)  # vec, dim k
                proc_t = proc.time()[3]
                tmp_u_mat = omg_u_mat(tmp_omega)        # slow step
                proc_t_acc[3] = proc_t_acc[3] + proc.time()[3] - proc_t
                tmp_u_vbt = omg_xu_obj_type(tmp_u_mat)  # vec, dim k

                # compute delta obj
                # delta_x_t = tmp_x_vbt - x_vbt_init
                # delta_u_t = tmp_u_vbt - u_vbt_init
                dx_mat[jnd, knd_cand] = delta_x_t =
                    tmp_x_vbt - x_vbt_init[knd_cand]
                du_mat[jnd, knd_cand] = delta_u_t =
                    tmp_u_vbt - u_vbt_init[knd_cand]
                delta_y = (1 - y_vec_init[jnd]) / val_m / val_k
                delta_d_t = data_type_specs$rate[knd_cand]

                # compute score
                score[jnd, knd_cand] = score_vec = (
                    (gamma_x * delta_x_t + gamma_u * delta_u_t) *
                    data_type_specs$weight[knd_cand] +
                    gamma_y * delta_y
                ) / delta_d_t
            } else if(num_cand > 0L){ # following selection
                knd_up_x = last_added_sensor[2]
                if(is.finite(score[jnd, knd_up_x])) {
                    # compute obj
                    tmp_omega = omg_omega_mat(
                        val_m, val_n, 1L,
                        placement_frame, tmp_w[, knd_up_x, drop = FALSE]
                    )
                    proc_t = proc.time()[3]
                    tmp_x_0_mat = omg_x_0_mat(tmp_omega) # slow step
                    proc_t_acc[1] = proc_t_acc[1] + proc.time()[3] - proc_t
                    proc_t = proc.time()[3]
                    tmp_x_cur = omg_x_mat(
                        simu_n              = 0L,
                        x_0_frame_mat       = tmp_x_0_mat,
                        arg_x_0_mat_history = array(0, dim = c(val_m, 1L, 1L)),
                        arg_s_impact_mat    = s_impact_mat[knd_up_x],   # global
                        arg_t_impact_mat    = t_impact_mat[knd_up_x]    # global
                    ) # slow step
                    proc_t_acc[2] = proc_t_acc[2] + proc.time()[3] - proc_t
                    tmp_x_mat = 1 - (1 - x_mat_prev[, knd_up_x]) * (1 - tmp_x_cur)
                    tmp_x_vbt = omg_xu_obj_type(tmp_x_mat)  # vec, dim k
                    dx_mat[jnd, knd_up_x] = tmp_x_vbt - x_vbt_init[knd_up_x]

                    proc_t = proc.time()[3]
                    if(placement_frame[jnd] ==
                       placement_frame[last_added_sensor[1]]) {
                        tmp_u_mat = omg_u_mat(tmp_omega)    # slow step
                        proc_t_acc[3] = proc_t_acc[3] + proc.time()[3] - proc_t
                        tmp_u_vbt = omg_xu_obj_type(tmp_u_mat)  # vec, dim k
                        du_mat[jnd, knd_up_x] = tmp_u_vbt - u_vbt_init[knd_up_x]
                    } else {
                        proc_t_acc[3] = proc_t_acc[3] + proc.time()[3] - proc_t
                    }
                }
                # compute delta obj
                delta_x_t = dx_mat[jnd, knd_cand]
                delta_u_t = du_mat[jnd, knd_cand]
                delta_y = (1 - y_vec_init[jnd]) / val_m / val_k
                delta_d_t = data_type_specs$rate[knd_cand]

                # compute score
                score[jnd, knd_cand] = score_vec = (
                    (gamma_x * delta_x_t + gamma_u * delta_u_t) *
                        data_type_specs$weight[knd_cand] +
                        gamma_y * delta_y
                ) / delta_d_t
            } # ENDIF

            # paranoid check
            # tmp_omega_bak = omg_omega_mat(
            #     val_m, val_n, val_k,
            #     placement_frame, tmp_w
            # )
            # tmp_x_0_mat_bak = omg_x_0_mat(tmp_omega_bak)
            # tmp_x_cur_bak = omg_x_mat(
            #     simu_n              = 0L,
            #     x_0_frame_mat       = tmp_x_0_mat_bak,
            #     arg_x_0_mat_history = array(0, dim = c(val_m, val_k, 1L)),
            #     arg_s_impact_mat    = s_impact_mat, # global
            #     arg_t_impact_mat    = t_impact_mat  # global
            # ) # slow step
            # tmp_x_mat_bak = 1 - (1 - x_mat_prev) * (1 - tmp_x_cur_bak)
            # tmp_x_vbt_bak = omg_xu_obj_type(tmp_x_mat_bak)
            # tmp_u_mat_bak = omg_u_mat(tmp_omega_bak)
            # tmp_u_vbt_bak = omg_xu_obj_type(tmp_u_mat_bak)

            # paranoid check, computer backup score
            # dx_t_bak = tmp_x_vbt_bak - x_vbt_init
            # du_t_bak = tmp_u_vbt_bak - u_vbt_init
            # dy_bak = (1 - y_vec_init[jnd]) / val_m / val_k
            # s_vc_bak = ((gamma_x * dx_t_bak + gamma_u * du_t_bak) *
            #     data_type_specs$weight + gamma_y * dy_bak
            # ) / data_type_specs$rate
            # stopifnot(abs(score[jnd, knd_vali] - s_vc_bak[knd_vali]) < 1e-14)

            # update maximum valid score, alternative approach
            # for(knd in knd_vali) {
            #     if(score[jnd, knd] > max_score &&
            #        d_val_init + data_type_specs$rate[knd] <= data_quota) {
            #         max_score = score[jnd, knd]
            #         max_c = c(
            #             jnd = jnd,
            #             knd = knd,
            #             ind = which(placement_frame[jnd, ] == 1)[1]
            #         )
            #     }
            # }

            # update maximum valid score
            quota_chk = (d_val_init + data_type_specs$rate <= this_quota)
            for(knd in knd_vali) {
                if(score[jnd, knd] > max_score && quota_chk[knd]) {
                    max_score = score[jnd, knd]
                    max_c = c(
                        jnd = jnd,
                        knd = knd,
                        ind = which(placement_frame[jnd, ] == 1)[1]
                    )
                }
            }
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
        if(is.null(max_c)) break
        if(verbose) cat(
            sprintf("    Iteration sum = %d,", sum(mat_w)),
            sprintf("[%d,", max_c[3]),
            sprintf("%d,", max_c[1]),
            sprintf("%d],", max_c[2]),
            sprintf("scr = %.2e,", max_score),
            sprintf("proc_t = %.0f %.0f %.0f msec\n",
                    1000 * proc_t_acc[1],
                    1000 * proc_t_acc[2],
                    1000 * proc_t_acc[3]
            )
        )

        # update the solution with the chosen sensor
        mat_w[max_c[1], max_c[2]] = 1
        num_chosen = num_chosen + 1L
    } # ENDWHILE

    # print(score)
    if(verbose) cat("...", "")
    greedy_1_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

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
