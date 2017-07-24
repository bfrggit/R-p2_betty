# lyap_grd.R
#
# Created: 2017-7-23
#  Coding: Charles Zhu
#
if(!exists("EX_LYAP_GRD_R")) {
    EX_LYAP_GRD_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

calc_work_mat_lyap_grd <<- function(
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
    gamma_y,
    gamma_l                 # coefficient of delta_delta_l
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
        lyap_grd_data_history               <<- rep(0, duration_frames + 1L)
        names(lyap_grd_data_history)        <<- dimnames(placement_roll)[[3]]
        lyap_grd_quota_history              <<- lyap_grd_data_history
        lyap_grd_quota_history[simu_n + 1L] <<- data_quota
    } else {
        lyap_grd_quota_history[simu_n + 1L] <<- data_quota +
            lyap_grd_quota_history[simu_n] -
            lyap_grd_data_history[simu_n]
    }
    this_quota = lyap_grd_quota_history[simu_n + 1L]

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
    x_mat_init = x_mat_prev
    u_mat_init = xu_mat_zero
    y_vec_init = omg_y_vec(mat_w)
    d_vec_init = omg_d_vec(data_type_specs, mat_w)
    d_val_init = sum(d_vec_init)

    omega_init = array(0, dim = c(val_m, val_n, val_k))
    dimnames(omega_init)[[1]] = z_nd_str("c", val_m)
    dimnames(omega_init)[[2]] = z_nd_str("n", val_n)
    dimnames(omega_init)[[3]] = z_nd_str("d", val_k)

    # initial score and delta obj
    # this solution is derived from greedy_2, but score needs to be redefined
    score = ifelse(capacity_mat > 0, NaN, -Inf)

    # initial delta obj for cells
    dx_mcl = xu_mat_ninf # delta obj mat for cells
    du_mcl = xu_mat_ninf
    x_vbt_rec = xu_mat_zero

    #-----------------------------------------------------------------------
    # all lines above together have been tested to take about 20 msec to run

    negative_n_0 = numeric(val_k)
    names(negative_n_0) = z_nd_str("d", val_k)

    # loop until all sensors (on all nodes) are active,
    #            maximum score is not positive, or
    #            data quota is used up
    proc_t_acc = c(0, 0, 0)
    last_added_sensor = NULL
    negative_jnd = NULL
    negative_acc = NULL
    negative_ind = negative_n_0

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

            # update omega
            omega_init[ind, jnd, knd] = 1

            # update coverage
            # x_0_mat_init is a binary mat
            if(x_0_mat_init[ind, knd] == 0) {
                x_mat_init[, knd] = 1 -
                    (1 - x_mat_init[, knd]) * (1 - s_impact_mat[[knd]][ind, ])
            }
            x_0_mat_init[ind, knd] = 1
            x_vbt_init[knd] = x_vbt_rec[ind, knd]

            # update util, using simplified func of objective_multi
            # only util of type knd in cell ind is affected
            u_mat_init[ind, knd] = local_util_f(omega_init[ind, , knd])

            # update y
            # only activation state of node jnd is affected
            y_vec_init[jnd] = 1

            # update d
            d_val_init = d_val_init + data_type_specs$rate[knd]
        } else {
            x_vbt_init = x_vbt_prev
        }
        u_vbt_init = omg_xu_obj_type(u_mat_init)
        dx_mcf = xu_mat_fals
        du_mcf = xu_mat_fals
        proc_t_res[3] = proc_t_res[3] + proc.time()[3] - proc_t

        max_score = -Inf
        max_c = NULL
        for(jnd in 1L:val_n) { # for each node
            proc_t = proc.time()[3]

            # attempt to switch on all sensors on-board
            tmp_w = mat_w
            if(!is.null(negative_jnd)) {
                tmp_w[negative_jnd, ] = negative_ind
            }
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
            vec_vl_x = vec_vali & !dx_mcf[ind, ]
            vec_vl_u = vec_vali & !du_mcf[ind, ]
            knd_vl_x = which(vec_vl_x)
            knd_vl_u = which(vec_vl_u)

            # get candidate of sensors that need their scores updated
            # candidates must be valid
            if(is.null(last_added_sensor) || jnd == last_added_sensor[1]) {
                knd_cand = knd_vali
                knd_cd_x = knd_vl_x
                knd_cd_u = knd_vl_u
            } else {
                knl = last_added_sensor[2]
                if(is.finite(score[jnd, knl])) {
                    knd_cand = knl
                } else knd_cand = integer(0)
                if(vec_vl_x[knl]) {
                    knd_cd_x = knl
                } else knd_cd_x = integer(0)
                if(vec_vl_u[knl]) {
                    knd_cd_u = knl
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
                        tmp_x_mat = x_mat_init[, knd_cd_x, drop = FALSE]
                        for(knt in 1L:length(knd_cd_x)) {
                            tmp_x_mat[, knt] = 1 -
                                (1 - tmp_x_mat[, knt]) *
                                (1 - s_impact_mat[[knd_cd_x[knt]]][ind, ])
                        }
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
                } else { # following selection
                    knd_up_x = last_added_sensor[2]
                    if(is.finite(score[jnd, knd_up_x])) {
                        # compute delta x for cells
                        if(knd_up_x %in% knd_cd_x) {
                            tmp_x_vec_k1 = x_mat_init[, knd_up_x]
                            if(x_0_mat_init[ind, knd_up_x] == 0) {
                                tmp_x_vec_k1 = 1 -
                                    (1 - tmp_x_vec_k1) *
                                    (1 - s_impact_mat[[knd_up_x]][ind, ])
                            }
                            tmp_x_vbt_k1 = mean(tmp_x_vec_k1)
                            dx_mcl[ind, knd_up_x] =
                                tmp_x_vbt_k1 - x_vbt_init[knd_up_x]
                            dx_mcf[ind, knd_up_x] = TRUE
                            x_vbt_rec[ind, knd_up_x] = tmp_x_vbt_k1
                        }

                        # compute delta u for cells
                        if(ind == last_added_sensor[3] &&
                           knd_up_x %in% knd_cd_u) {
                            tmp_omega_k1 = omega_init[, , knd_up_x]
                            tmp_omega_k1[ind, jnd] = 1
                            tmp_u_vec_k1 = omg_u_vec_k1(tmp_omega_k1)
                            tmp_u_vbt_k1 = mean(tmp_u_vec_k1)
                            du_mcl[ind, knd_up_x] =
                                tmp_u_vbt_k1 - u_vbt_init[knd_up_x]
                            du_mcf[ind, knd_up_x] = TRUE
                        }
                    }
                    if(jnd == last_added_sensor[1]) score_update = TRUE
                } # ENDIF
            } # ENDIF

            # compute delta obj
            delta_x_t = dx_mcl[ind, knd_vali]
            delta_u_t = du_mcl[ind, knd_vali]
            delta_y = (1 - y_vec_init[jnd]) / val_m / val_k
            delta_d_t = data_type_specs$rate[knd_vali]
            delta_delta_l_t = 0
            if(is.finite(this_quota)) {
                delta_delta_l_t = delta_d_t * (
                    this_quota - d_val_init - d_val_init - delta_d_t
                ) / data_quota
            }

            # compute score
            score[jnd, knd_vali] = (gamma_x * delta_x_t + gamma_u * delta_u_t) *
                    data_type_specs$weight[knd_vali] +
                gamma_y * delta_y +
                gamma_l * delta_delta_l_t

            # update maximum valid score
            for(knd in knd_vali) {
                if(score[jnd, knd] > max_score) {
                    max_score = score[jnd, knd]
                    max_c = c(jnd = jnd, knd = knd, ind = ind)
                }
            }
            proc_t_res[1] = proc_t_res[1] + proc.time()[3] - proc_t
        } # ENDFOR

        # negative score handler
        stopifnot(!is.null(max_c))
        inc = max_c[3]
        jnc = max_c[1]
        knc = max_c[2]
        if(max_score <= 0) {
            if(is.null(negative_jnd)) {
                negative_jnd = jnc
                negative_acc = max_score
                negative_ind[knc] = 1
            } else max_c = NULL
        } else { # positive score handler
            if(!is.null(negative_jnd)) {
                stopifnot(jnc == negative_jnd)
                negative_acc = negative_acc + max_score
                negative_ind[knc] = 1
                if(negative_acc >= 0) {
                    mat_w[negative_jnd, ] = negative_ind
                    negative_jnd = NULL
                    negative_acc = NULL
                    negative_ind = negative_n_0
                }
            } else mat_w[jnc, knc] = 1
        }
        last_added_sensor = max_c
        proc_t_acc = proc_t_acc + proc_t_res

        if(verbose && any(proc_t_res >= 4e-2)){
            cat(
                sprintf("    Iteration sum = %d,", num_chosen),
                sprintf("proc_t = %.0f %.0f msec",
                    1000 * proc_t_res[2],
                    1000 * proc_t_res[1]
                )
            )
            if(is.null(max_c)) {
                cat("\n")
                break
            }
            cat(
                sprintf(", [%d %d %d],", inc, jnc, knc),
                sprintf("scr = %.2e\n", max_score)
            )
        } else if(is.null(max_c)) break

        # update the solution with the chosen sensor
        num_chosen = num_chosen + 1L
    } # ENDWHILE

    # print(score)
    if(verbose) {
        cat(
            # sprintf("    Iteration end = %d,", num_chosen),
            sprintf("    Iteration end = %d,", sum(mat_w)),
            sprintf("proc_t = %.0f %.0f %.0f msec",
                    1000 * proc_t_acc[3],
                    1000 * proc_t_acc[2],
                    1000 * proc_t_acc[1]
            ),
            "...", ""
        )
    }
    lyap_grd_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

    mat_w # RETURN
}

get_calc_work_mat_lyap_grd_f <<- function(
    gamma_x,
    gamma_u,
    gamma_y,
    gamma_l
){
    function(...) {
        calc_work_mat_lyap_grd(
            ...,
            gamma_x = gamma_x,
            gamma_u = gamma_u,
            gamma_y = gamma_y,
            gamma_l = gamma_l
        )
    } # RETURN
}

} # ENDIF
