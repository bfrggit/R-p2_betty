# dummy.R
#
# Author: Charles Zhu

if(!exists("EX_DUMMY_R")) {
    EX_DUMMY_R <<- TRUE

get_placement_mat_rand <<- function(val_n, val_m) {
    stopifnot(is.integer(val_n))
    stopifnot(is.integer(val_m))
    stopifnot(val_n > 0)
    stopifnot(val_m > 0)

    mat_g = matrix(
        0,
        nrow = val_n,
        ncol = val_m,
        byrow = TRUE
    )
    rownames(mat_g) = paste("n", as.character(1:val_n), sep = "_")
    colnames(mat_g) = paste("c", as.character(1:val_m), sep = "_")
    placement_per_node = as.integer(runif(val_n, min = 1, max = val_m + 1))
    for(jnd in 1:val_n) {
        mat_g[jnd, placement_per_node[jnd]] = 1
    }
    mat_g # RETURN
}

} # ENDIF
