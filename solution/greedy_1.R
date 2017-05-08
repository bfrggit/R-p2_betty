# greedy_1.R
#
# Created: 2017-4-30
# Updated: 2017-5-8
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

    # override verbose msg
    # verbose = FALSE

    # prepare data for quick access
    num_sensor = sum(capacity_mat)
    placement_roll = get_placement_f()
    placement_frame = placement_roll[, , simu_n + 1L]
    duration_frames = dim(placement_roll)[3] - 1L
    mat_score_zero = mat_w # for dimension names, contents are disregarded
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    xu_mat_ninf = matrix(-Inf, nrow = val_m, ncol = val_k)
    xu_mat_fals = matrix(FALSE, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = rownames(xu_mat_ninf) = rownames(xu_mat_fals) =
        z_nd_str("c", val_m)
    colnames(xu_mat_zero) = colnames(xu_mat_ninf) = colnames(xu_mat_fals) =
        z_nd_str("d", val_k)
    placement_vec = placement_frame %*% (1L:val_m)

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
    x_vbt_prev = omg_xu_obj_type(x_mat_prev)

    # initial val of obj maintained in loops
    x_0_mat_init = xu_mat_zero
    # x_mat_init = x_mat_prev
    u_mat_init = xu_mat_zero
    y_vec_init = omg_y_vec(mat_w)
    d_vec_init = omg_d_vec(data_type_specs, mat_w)
    d_val_init = sum(d_vec_init)

    omega_init = array(0, dim = c(val_m, val_n, val_k))
    dimnames(omega_init)[[1]] = z_nd_str("c", val_m)
    dimnames(omega_init)[[2]] = z_nd_str("n", val_n)
    dimnames(omega_init)[[3]] = z_nd_str("d", val_k)

    # initial score and delta obj
    score = ifelse(capacity_mat > 0, NaN, -Inf)
    # dx_mat = ifelse(capacity_mat > 0, 0, -Inf)
    # du_mat = dx_mat
    # dy_vec = ifelse(rowSums(capacity_mat) > 0, 1, +Inf)

    # initial delta obj for cells
    dx_mcl = xu_mat_ninf # delta obj mat for cells
    du_mcl = xu_mat_ninf
    x_vbt_rec = xu_mat_zero

    #-----------------------------------------------------------------------
    # all lines above together have been tested to take about 20 msec to run

    # loop until all sensors (on all nodes) are active,
    #            maximum score is not positive, or
    #            data quota is used up
    proc_t_acc = c(0, 0, 0)
    last_added_sensor = NULL
    num_chosen = 0L
    if(verbose) cat("...", "\n")
    while(num_chosen < num_sensor) {
        # paranoid check
        # stopifnot(num_chosen == sum(mat_w))

        proc_t_res = c(0, 0, 0)
        proc_t = proc.time()[3]

        # update obj to reflect last added sensor
        if(!is.null(last_added_sensor)) {
            jnd = last_added_sensor[1]  # index of added node
            knd = last_added_sensor[2]  # index of adder sensor
            ind = last_added_sensor[3]  # cell index of node jnd

            score[jnd, knd] = -Inf
            # dx_mat[jnd, knd] = -Inf
            # du_mat[jnd, knd] = -Inf

            # update omega
            omega_init[ind, jnd, knd] = 1
            # omega_mask = omega_init[, , knd, drop = FALSE]

            # update coverage, alternative approach
            # x_0_mat_init[ind, knd] = 1
            # x_vec_mask = omg_x_vec_k1_fr1(
            #     x_0_frame_vec_k1 = x_0_mat_init[, knd],
            #     arg_s_impact_mat_k1 = s_impact_mat[[knd]],
            #     arg_t_impact_mat_k1 = t_impact_mat[[knd]]
            # )
            # x_mat_init[, knd] = 1 - (1 - x_mat_prev[, knd]) * (1 - x_vec_mask)

            # paranoid check
            # x_0_mat_mask_bak = omg_x_0_mat(omega_mask)
            # x_mat_mask_bak = omg_x_mat(
            #     simu_n              = 0L,
            #     x_0_frame_mat       = x_0_mat_mask_bak,
            #     arg_x_0_mat_history = array(0, dim = c(val_m, 1L, 1L)),
            #     arg_s_impact_mat    = s_impact_mat[knd],    # should be a list
            #     arg_t_impact_mat    = t_impact_mat[knd]     # should be a list
            # ) # m by k, k = 1
            # x_vec_init_bak = 1 - (1 - x_mat_prev[, knd]) * (1 - x_mat_mask_bak)
            # stopifnot(x_0_mat_init[, knd] == x_0_mat_mask_bak)
            # stopifnot(x_vec_mask == x_mat_mask_bak)
            # stopifnot(x_mat_init[, knd] == x_vec_init_bak)

            # update coverage
            x_0_mat_init[ind, knd] = 1
            x_vbt_init[knd] = x_vbt_rec[ind, knd]

            # update util, using simplified func of objective_multi
            # only util of type knd in cell ind is affected
            # u_mat_mask = local_util_f(omega_mask[ind, , 1L])
            # u_mat_init[ind, knd] = u_mat_mask
            u_mat_init[ind, knd] = local_util_f(omega_init[ind, , knd])

            # update y
            # only activation state of node jnd is affected
            y_vec_init[jnd] = 1

            # update d
            d_val_init = d_val_init + data_type_specs$rate[knd]
        } else {
            x_vbt_init = x_vbt_prev
        }
        # paranoid check
        # x_vbt_init_bak = omg_xu_obj_type(x_mat_init)
        # stopifnot(x_vbt_init == x_vbt_init_bak)

        u_vbt_init = omg_xu_obj_type(u_mat_init)
        dx_mcf = xu_mat_fals
        du_mcf = xu_mat_fals
        proc_t_res[3] = proc_t_res[3] + proc.time()[3] - proc_t

        max_score = 0
        max_c = NULL
        for(jnd in 1L:val_n) { # for each node
            proc_t = proc.time()[3]

            # attempt to switch on all sensors on-board
            tmp_w = mat_w
            tmp_w[jnd, ] = capacity_mat[jnd, ]

            # it is very important to know where the last added sensor is
            # ind = which(placement_frame[jnd, ] == 1)[1]
            ind = placement_vec[jnd]

            # get list of valid sensors to compare scores
            # active sensors in mat_w are not valid
            # only valid sensors can be activated
            vec_vali = is.finite(score[jnd, ]) | is.nan(score[jnd, ])
            knd_vali = which(vec_vali)
            num_vali = length(knd_vali)
            if(num_vali <= 0L) next
            knd_vl_x = which(vec_vali & !dx_mcf[ind, ])
            knd_vl_u = which(vec_vali & !du_mcf[ind, ])

            # paranoid check
            # knd_vali_bak = which(tmp_w[jnd, ] - mat_w[jnd, ] > 0)
            # stopifnot(knd_vali == knd_vali_bak)

            # get candidate of sensors that need their scores updated
            # candidates must be valid
            if(is.null(last_added_sensor) || jnd == last_added_sensor[1]) {
                knd_cand = knd_vali
                knd_cd_x = knd_vl_x
                knd_cd_u = knd_vl_u
            } else {
                knd_cand = last_added_sensor[2]
                if(!is.finite(score[jnd, knd_cand])) knd_cand = integer(0)
                if(last_added_sensor[2] %in% knd_vl_x) {
                    knd_cd_x = last_added_sensor[2]
                } else knd_cd_x = integer(0)
                if(last_added_sensor[2] %in% knd_vl_u) {
                    knd_cd_u = last_added_sensor[2]
                } else knd_cd_u = integer(0)
            }
            num_cand = length(knd_cand)
            num_cd_x = length(knd_cd_x)
            num_cd_u = length(knd_cd_u)
            proc_t_res[2] = proc_t_res[2] + proc.time()[3] - proc_t
            proc_t = proc.time()[3]
            if(num_cand > 0L) {
                if(is.null(last_added_sensor)){ # first selection
                    # compute delta x for cells
                    if(num_cd_x > 0L) {
                        tmp_x_0_mat = x_0_mat_init[, knd_cd_x, drop = FALSE]
                        tmp_x_0_mat[ind, ] = 1
                        tmp_x_cur = omg_x_mat(
                            simu_n              = 0L,
                            x_0_frame_mat       = tmp_x_0_mat,
                            arg_x_0_mat_history = array(0,
                                dim = c(val_m, num_cd_x, 1L)),
                            arg_s_impact_mat    = s_impact_mat[knd_cd_x],
                            arg_t_impact_mat    = t_impact_mat[knd_cd_x]
                        )
                        tmp_x_mat = 1 -
                            (1 - x_mat_prev[, knd_cd_x]) * (1 - tmp_x_cur)
                        tmp_x_vbt = omg_xu_obj_type(tmp_x_mat)
                        dx_mcl[ind, knd_cd_x] = tmp_x_vbt - x_vbt_init[knd_cd_x]
                        dx_mcf[ind, knd_cd_x] = TRUE
                        x_vbt_rec[ind, knd_cd_x] = tmp_x_vbt
                    }

                    # compute delta u for cells
                    if(num_cd_u > 0L) {
                        tmp_u_mat = u_mat_init[, knd_cd_u, drop = FALSE]
                        tmp_u_mat[ind, ] = local_util_f(1)
                        tmp_u_vbt = omg_xu_obj_type(tmp_u_mat)
                        du_mcl[ind, knd_cd_u] = tmp_u_vbt - u_vbt_init[knd_cd_u]
                        du_mcf[ind, knd_cd_u] = TRUE
                    }

                    # compute in old fashion for paranoid check
                    # tmp_x_0_mat_bak = x_0_mat_init[, knd_cand]
                    # tmp_x_0_mat_bak[ind, ] = 1 # local coverage should be 1
                    # tmp_x_cur_bak = omg_x_mat(
                    #     simu_n              = 0L,
                    #     x_0_frame_mat       = tmp_x_0_mat_bak,
                    #     arg_x_0_mat_history = array(0,
                    #         dim = c(val_m, num_cand, 1L)),
                    #     arg_s_impact_mat    = s_impact_mat[knd_cand], # global
                    #     arg_t_impact_mat    = t_impact_mat[knd_cand]  # global
                    # ) # slow step
                    # tmp_x_mat_bak = 1 -
                    #     (1 - x_mat_prev[, knd_cand]) * (1 - tmp_x_cur_bak)
                    # tmp_x_vbt_bak = omg_xu_obj_type(tmp_x_mat_bak)
                    # dx_mat[jnd, knd_cand] =
                    #     tmp_x_vbt_bak - x_vbt_init[knd_cand]
                    #
                    # tmp_u_mat_bak = u_mat_init[, knd_cand]
                    # tmp_u_mat_bak[ind, ] = local_util_f(1)
                    # tmp_u_vbt_bak = omg_xu_obj_type(tmp_u_mat_bak)
                    # du_mat[jnd, knd_cand] =
                    #     tmp_u_vbt_bak - u_vbt_init[knd_cand]
                } else { # following selection
                    knd_up_x = last_added_sensor[2]
                    if(is.finite(score[jnd, knd_up_x])) {
                        # compute obj using single-case func
                        # tmp_x_0_vec_k1 = x_0_mat_init[, knd_up_x]
                        # tmp_x_0_vec_k1[ind] = 1
                        # tmp_x_cur_k1 = omg_x_vec_k1_fr1(
                        #     x_0_frame_vec_k1    = tmp_x_0_vec_k1,
                        #     arg_s_impact_mat_k1 = s_impact_mat[[knd_up_x]],
                        #     arg_t_impact_mat_k1 = t_impact_mat[[knd_up_x]]
                        # )
                        # tmp_x_vec_k1 = 1 -
                        #     (1 - x_mat_prev[, knd_up_x]) * (1 - tmp_x_cur_k1)
                        # tmp_x_vbt_k1 = mean(tmp_x_vec_k1)
                        # dx_mat[jnd, knd_up_x] =
                        #     tmp_x_vbt_k1 - x_vbt_init[knd_up_x]

                        # compute delta x for cells
                        if(knd_up_x %in% knd_cd_x) {
                            tmp_x_0_vec_k1 = x_0_mat_init[, knd_up_x]
                            tmp_x_0_vec_k1[ind] = 1
                            tmp_x_cur_k1 = omg_x_vec_k1_fr1(
                                x_0_frame_vec_k1    = tmp_x_0_vec_k1,
                                arg_s_impact_mat_k1 = s_impact_mat[[knd_up_x]],
                                arg_t_impact_mat_k1 = t_impact_mat[[knd_up_x]]
                            )
                            tmp_x_vec_k1 = 1 -
                                (1 - x_mat_prev[, knd_up_x]) *
                                (1 - tmp_x_cur_k1)
                            tmp_x_vbt_k1 = mean(tmp_x_vec_k1)
                            dx_mcl[ind, knd_up_x] =
                                tmp_x_vbt_k1 - x_vbt_init[knd_up_x]
                            dx_mcf[ind, knd_up_x] = TRUE
                            x_vbt_rec[ind, knd_up_x] = tmp_x_vbt_k1
                        }

                        # if(ind == last_added_sensor[3]) {
                        #     tmp_omega_k1 = omg_omega_mat_k1(
                        #         val_m, val_n,
                        #         placement_frame, tmp_w[, knd_up_x]
                        #     )
                        #     tmp_u_vec_k1 = omg_u_vec_k1(tmp_omega_k1)
                        #     tmp_u_vbt_k1 = mean(tmp_u_vec_k1)
                        #     du_mat[jnd, knd_up_x] =
                        #         tmp_u_vbt_k1 - u_vbt_init[knd_up_x]
                        # }

                        # compute delta u for cells
                        if(ind == last_added_sensor[3] &&
                           knd_up_x %in% knd_cd_u) {
                            tmp_omega_k1 = omega_init[, , knd_up_x]
                            tmp_omega_k1[ind, jnd] = 1

                            # paranoid check
                            # tmp_omega_k1_bak = omg_omega_mat_k1(
                            #     val_m, val_n,
                            #     placement_frame, tmp_w[, knd_up_x]
                            # )
                            # stopifnot(tmp_omega_k1 == tmp_omega_k1_bak)

                            tmp_u_vec_k1 = omg_u_vec_k1(tmp_omega_k1)
                            tmp_u_vbt_k1 = mean(tmp_u_vec_k1)
                            du_mcl[ind, knd_up_x] =
                                tmp_u_vbt_k1 - u_vbt_init[knd_up_x]
                            du_mcf[ind, knd_up_x] = TRUE
                        }
                    }
                    if(jnd == last_added_sensor[1]) score_update = TRUE
                } # ENDIF
                # paranoid check
                # stopifnot(dx_mat[jnd, knd_cand] == dx_mcl[ind, knd_cand])
                # stopifnot(du_mat[jnd, knd_cand] == du_mcl[ind, knd_cand])

                # compute delta obj
                # delta_x_t = dx_mat[jnd, knd_cand]
                # delta_u_t = du_mat[jnd, knd_cand]
                delta_x_t = dx_mcl[ind, knd_cand]
                delta_u_t = du_mcl[ind, knd_cand]
                delta_y = (1 - y_vec_init[jnd]) / val_m / val_k
                delta_d_t = data_type_specs$rate[knd_cand]

                # compute score
                score[jnd, knd_cand] = score_new = (
                    (gamma_x * delta_x_t + gamma_u * delta_u_t) *
                        data_type_specs$weight[knd_cand] +
                        gamma_y * delta_y
                ) / delta_d_t
            } # ENDIF

            # update maximum valid score
            quota_chk = (d_val_init + data_type_specs$rate <= this_quota)
            for(knd in knd_vali) {
                if(score[jnd, knd] > max_score && quota_chk[knd]) {
                    max_score = score[jnd, knd]
                    max_c = c(jnd = jnd, knd = knd, ind = ind)
                }
            }
            proc_t_res[1] = proc_t_res[1] + proc.time()[3] - proc_t
        } # ENDFOR
        last_added_sensor = max_c
        # stopifnot(num_chosen == sum(mat_w))
        proc_t_acc = proc_t_acc + proc_t_res

        if(verbose && any(proc_t_res >= 2e-2)){
            cat(
                sprintf("    Iteration sum = %d,", num_chosen),
                sprintf("proc_t = %.0f %.0f %.0f msec",
                    1000 * proc_t_res[3],
                    1000 * proc_t_res[2],
                    1000 * proc_t_res[1]
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
            sprintf("    Iteration end = %d,", num_chosen),
            sprintf("proc_t = %.0f %.0f %.0f msec",
                    1000 * proc_t_acc[3],
                    1000 * proc_t_acc[2],
                    1000 * proc_t_acc[1]
            ),
            "...", ""
        )
    }
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
