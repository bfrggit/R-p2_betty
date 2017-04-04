# objective_multi.R
#
# Author: Charles Zhu

if(!exists("EX_OBJECTIVE_MULTI_R")) {
    EX_OBJECTIVE_MULTI_R <<- TRUE

objective_zero <<- function() {
    obj_zero = data.frame(
        0,
        0,
        0,
        0,
        0,
        row.names = c("zero"),
        check.names = TRUE,
        fix.empty.names = TRUE
    )
    colnames(obj_zero) = c("overall", "cover", "util", "traffic", "nact")

    obj_zero # RETURN
}

get_objective_zero_f <<- function() {
    function(...) {
        objective_zero() # RETURN
    } # RETURN
}

objective_multi <<- function(
    t_frane,
    simu_n,
    val_n,
    val_m,
    val_k,
    grid,
    data_type_specs,
    capacity_mat,
    get_placement_f,
    work_mat_history,
    verbose = FALSE
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    # FIXME: not implemented
    objective_zero()
}

get_objective_multi_f <<- function() {
    function(...) {
        objective_multi(...) # RETURN
    } # RETURN
}

} # ENDIF
