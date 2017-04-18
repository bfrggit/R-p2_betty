# calc_work_fill_0.R
#
# Created: As part of the initial version of the project
# Updated: 2017-4-17
#  Author: Charles Zhu
#
if(!exists("EX_CALC_WORK_FILL_0_R")) {
    EX_CALC_WORK_FILL_0_R <<- TRUE

    source("lib/basic.R")

calc_work_mat_fill_0 <<- function(
    t_frame,                # time frame length, sec
    simu_n,                 # time frame number, on which simulation is running
    val_n,
    val_m,
    val_k,
    grid,                   # unused in this example implementation
    data_type_specs,        # unused in this example implementation
    capacity_mat,           # unused in this example implementation
    get_placement_f,        # unused in this example implementation
    local_util_f,           # unused in this example implementation
    data_quota,             # unused in this example implementation
    verbose = FALSE
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    mat_w = matrix(
        0,
        nrow = val_n,
        ncol = val_k,
        byrow = TRUE
    )
    rownames(mat_w) = z_nd_str("n", val_n)
    colnames(mat_w) = z_nd_str("d", val_k)

    mat_w # RETURN
}

}
