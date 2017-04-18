# element_rand.R
#
# Created: As part of the initial version of the project
# Updated: 2017-4-17
#  Author: Charles Zhu
#
if(!exists("EX_ELEMENT_RAND_R")) {
    EX_ELEMENT_RAND_R <<- TRUE

    source("lib/basic.R")

# GENERATE RANDOM sensing capacity matrix for all nodes
get_capacity_mat_rand <<- function(val_n, val_k, p) {
    stopifnot(is.integer(val_n))
    stopifnot(is.integer(val_k))
    stopifnot(length(val_n) == 1L)
    stopifnot(length(val_k) == 1L)
    stopifnot(val_n > 0L)
    stopifnot(val_k > 0L)

    stopifnot(is.numeric(p))    # p is the probability of ONE assignments
    stopifnot(length(p) == 1L)
    stopifnot(p > 0 && p <= 1)

    mat_c = matrix(
        0,
        nrow = val_n,
        ncol = val_k,
        byrow = TRUE
    )
    rownames(mat_c) = z_nd_str("n", val_n)
    colnames(mat_c) = z_nd_str("d", val_k)
    c_t = runif(val_n * val_k, min = 0, max = 1)
    c_p = ifelse(c_t > p, 0, 1)
    mat_c[] = c_p

    mat_c # RETURN
}

# GENERATE RANDOM data type specs
get_data_type_spec_df_rand <<- function(val_k, r_max) {
    stopifnot(is.integer(val_k))
    stopifnot(length(val_k) == 1L)
    stopifnot(val_k > 0L)

    stopifnot(is.numeric(r_max))    # r_max is the maximum data rate
    stopifnot(length(r_max) == 1L)
    stopifnot(r_max > 0)

    df_d = data.frame(
        rate = rep(0, val_k),
        weight = rep(0, val_k),
        s_impact_f = z_nd_str("s_impact_f", val_k),
        t_impact_f = z_nd_str("t_impact_f", val_k),
        row.names = z_nd_str("d", val_k),
        check.names = TRUE,
        fix.empty.names = TRUE,
        stringsAsFactors = FALSE
    )
    df_d[, "rate"] = runif(val_k, min = 0, max = r_max)
    pr_t = runif(val_k)
    df_d[, "weight"] = pr_t / sum(pr_t)

    df_d # RETURN
}

} # ENDIF
