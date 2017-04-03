# calc_work_fill_1.R
#
# Author: Charles Zhu

if(!exists("EX_CALC_WORK_FILL_1_R")) {
    EX_CALC_WORK_FILL_1_R <<- TRUE

    source("lib/basic.R")

calc_work_mat_fill_1 <<- function(
    t_frame,                # time frame length, sec
    simu_n,                 # time frame number, on which simulation is running
    val_n,
    val_m,
    val_k,
    grid,
    data_type_specs,
    capacity_mat,
    get_placement_f,
    verbose = FALSE
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    mat_w = matrix(
        1,
        nrow = val_n,
        ncol = val_k,
        byrow = TRUE
    )
    rownames(mat_w) = z_nd_str("n", val_n)
    colnames(mat_w) = z_nd_str("d", val_k)

    mat_w = mat_w * capacity_mat # RETURN
}

}
