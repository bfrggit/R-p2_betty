# grd_2_ut.R
#
# Created: 2017-6-1
# Updated: 2017-6-2
#  Coding: Charles Zhu
#
if(!exists("EX_GRD_2_UT_R")) {
    EX_GRD_2_UT_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

# this limited version of greedy_2 provides partial and incomplete evalutaion
# for objective functions. used for comparison with the complete version
calc_work_mat_grd_2_ut <<- function(
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
        grd_2_ut_data_history               <<- rep(0, duration_frames + 1L)
        names(grd_2_ut_data_history)        <<- dimnames(placement_roll)[[3]]
        grd_2_ut_quota_history              <<- grd_2_ut_data_history
        grd_2_ut_quota_history[simu_n + 1L] <<- data_quota
    } else {
        grd_2_ut_quota_history[simu_n + 1L] <<- data_quota +
            grd_2_ut_quota_history[simu_n] -
            grd_2_ut_data_history[simu_n]
    }
    this_quota = grd_2_ut_quota_history[simu_n + 1L]

    # initial val of obj maintained in loops
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

    # initial delta obj for cells
    du_mcl = xu_mat_ninf

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

            # update util, using simplified func of objective_multi
            # only util of type knd in cell ind is affected
            u_mat_init[ind, knd] = local_util_f(omega_init[ind, , knd])

            # update y
            # only activation state of node jnd is affected
            y_vec_init[jnd] = 1

            # update d
            d_val_init = d_val_init + data_type_specs$rate[knd]
        } else {
            # x_vbt_init = x_vbt_prev
        }
        u_vbt_init = omg_xu_obj_type(u_mat_init)
        du_mcf = xu_mat_fals
        quota_chk = (d_val_init + data_type_specs$rate <= this_quota)
        knd_chkd = which(quota_chk)
        proc_t_res[3] = proc_t_res[3] + proc.time()[3] - proc_t

        if(verbose){
            if(length(knd_chkd) <= 0L) {
                cat(
                    sprintf("    Iteration sum = %d,", num_chosen),
                    sprintf("quota reached\n")
                )
                break
            }
        } else if(length(knd_chkd) <= 0L) break

        max_score = 0
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
            vec_vl_u = vec_vali & !du_mcf[ind, ]
            knd_vl_u = which(vec_vl_u)

            # get candidate of sensors that need their scores updated
            # candidates must be valid
            if(is.null(last_added_sensor) || jnd == last_added_sensor[1]) {
                knd_cand = knd_vali
                knd_cd_u = knd_vl_u
            } else {
                knl = last_added_sensor[2]
                if(is.finite(score[jnd, knl])) {
                    knd_cand = knl
                } else knd_cand = integer(0)
                if(vec_vl_u[knl]) {
                    knd_cd_u = knl
                } else knd_cd_u = integer(0)
            }
            num_cand = length(knd_cand)
            num_cd_u = length(knd_cd_u)
            proc_t_res[2] = proc_t_res[2] + proc.time()[3] - proc_t
            proc_t = proc.time()[3]
            if(num_cand > 0L) {
                if(is.null(last_added_sensor)){ # first selection
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

                # compute delta obj
                delta_u_t = du_mcl[ind, knd_cand]
                delta_y = (1 - y_vec_init[jnd]) / val_m / val_k
                delta_d_t = data_type_specs$rate[knd_cand]

                # compute score
                score[jnd, knd_cand] = (
                    gamma_u * delta_u_t *
                        data_type_specs$weight[knd_cand] +
                        gamma_y * delta_y
                ) / delta_d_t
            } # ENDIF

            # update maximum valid score
            # quota_chk = (d_val_init + data_type_specs$rate <= this_quota)
            for(knd in knd_vali) {
                if(score[jnd, knd] > max_score && quota_chk[knd]) {
                    max_score = score[jnd, knd]
                    max_c = c(jnd = jnd, knd = knd, ind = ind)
                }
            }
            proc_t_res[1] = proc_t_res[1] + proc.time()[3] - proc_t
        } # ENDFOR

        # negative score handler
        if(is.null(max_c)) {
            if(is.null(negative_jnd)) {
                # stopifnot(all(score[, knd_chkd] < 0))
                # stopifnot(all(y_vec_init[which(
                #     apply(
                #         score[, knd_chkd],
                #         MARGIN = 1,
                #         FUN = function(vec) {
                #             any(is.finite(vec))
                #         }
                #     )
                # )] == 0))
                score_pos = score[, knd_chkd, drop = FALSE] -
                    matrix(
                        gamma_y * 1 / data_type_specs$rate[knd_chkd],
                        nrow = nrow(score), ncol = length(knd_chkd),
                        byrow = TRUE
                    )
                # stopifnot(all(score_pos >= 0 || is.infinite(score_pos)))
                # stopifnot(any(score_pos >= 0))
                chk_c = which(score_pos == max(score_pos), arr.ind = TRUE)[1, ]
                jnc = chk_c[1]
                knc = knd_chkd[chk_c[2]]
                inc = placement_vec[jnc]
                max_c = c(jnd = jnc, knd = knc, ind = inc)
                max_score = score[jnc, knc]
                negative_jnd = jnc
                negative_acc = score[jnc, knc] * data_type_specs$rate[knc]
                negative_ind[knc] = 1
            }
        } else { # positive score handler
            inc = max_c[3]
            jnc = max_c[1]
            knc = max_c[2]
            if(!is.null(negative_jnd)) {
                stopifnot(jnc == negative_jnd)
                negative_acc = negative_acc +
                    score[jnc, knc] * data_type_specs$rate[knc]
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
            sprintf("    Iteration end = %d,", num_chosen),
            sprintf("proc_t = %.0f %.0f %.0f msec",
                    1000 * proc_t_acc[3],
                    1000 * proc_t_acc[2],
                    1000 * proc_t_acc[1]
            ),
            "...", ""
        )
    }
    grd_2_ut_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

    mat_w # RETURN
}

get_calc_work_mat_grd_2_ut_f <<- function(
    gamma_u,
    gamma_y
){
    function(...) {
        calc_work_mat_grd_2_ut(
            ...,
            gamma_u = gamma_u,
            gamma_y = gamma_y
        )
    } # RETURN
}

} # ENDIF
