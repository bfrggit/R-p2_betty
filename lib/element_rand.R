# element_rand.R
#
# Author: Charles Zhu

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

    stopifnot(is.numeric(p))    # p is the probablity of ONE assignments
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
        rep(0, val_k),
        z_nd_str("s_impact", val_k),
        z_nd_str("t_impact", val_k),
        row.names = z_nd_str("d", val_k),
        check.names = TRUE,
        fix.empty.names = TRUE
    )
    colnames(df_d) = c("rate", "s_impact_f", "t_impact_f")
    df_d[,"rate"] = runif(val_k, min = 0, max = r_max)

    df_d # RETURN
}

# GENERATE LOCAL ONLY impact functions
make_impact_f_local_only <<- function(val_k) {
    stopifnot(is.integer(val_k))
    stopifnot(length(val_k) == 1L)
    stopifnot(val_k > 0L)

    for(knd in 1L:val_k) {
        do.call(
            "<<-",
            list(
                paste("s_impact_f", knd, sep = "_"),
                function(x) {
                    stopifnot(is.numeric(x))
                    stopifnot(x >= 0)

                    ifelse(x > 0, 0, 1)
                }
            )
        )
        do.call(
            "<<-",
            list(
                paste("t_impact_f", knd, sep = "_"),
                function(x) {
                    stopifnot(is.numeric(x))
                    stopifnot(x >= 0)

                    ifelse(x > 0, 0, 1)
                }
            )
        )
    }

    NA # NO RETURN
}

} # ENDIF
