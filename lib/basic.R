# basic.R
#
# Created: As part of the initial version of the project
#  Author: Charles Zhu
#
if(!exists("EX_BASIC_R")) {
    EX_BASIC_R <<- TRUE

# RETURN z_ notation strings
# z_nd_str <<- function(str_z, val_n) {
#     stopifnot(is.character(str_z))
#     stopifnot(is.integer(val_n))
#     stopifnot(length(val_n) == 1L)
#     stopifnot(length(str_z) == 1L)
#     stopifnot(val_n > 0L)
#
#     paste(str_z, as.character(1L:val_n), sep = "_") # RETURN
# }

# RETURN z_ notation strings constructed from a list
z_cl_str <<- function(str_z, val_c) {
    stopifnot(is.character(str_z))
    stopifnot(is.integer(val_c))
    stopifnot(length(val_c) > 0L)
    stopifnot(length(str_z) == 1L)

    paste(str_z, as.character(val_c), sep = "_") # RETURN
}

# RETURN z_ notation strings
z_nd_str <<- function(str_z, val_n) {
    stopifnot(is.integer(val_n))
    stopifnot(length(val_n) == 1L)
    stopifnot(val_n > 0L)

    z_cl_str(str_z, 1L:val_n) # RETURN
}

# PROTOTYPE of LOCAL ONLY impact functions
# proto_impact_local_only <<- function(x) {
#     stopifnot(is.numeric(x))
#     stopifnot(x >= 0)
#
#     ifelse(x > 0, 0, 1)
# }

# PROTOTYPE of STEP impact functions
proto_impact_step <<- function(x, step) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(step))
    stopifnot(length(step) == 1)
    stopifnot(x >= 0)

    ifelse(x > step, 0, 1)
}

# PROTOTYPE of EXP impact functions
proto_impact_exp <<- function(x, t_const) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(t_const))
    stopifnot(length(t_const) == 1)
    stopifnot(x >= 0)

    exp(-x / t_const) # RETURN
}

# PROTOTYPE of LOCAL ONLY impact functions
proto_impact_local_only <<- function(x) {
    proto_impact_step(x, 0)
}

# general GETTER of impact functions
get_impact_f_type <<- function(type, ...) {
    stopifnot(is.character(type))
    stopifnot(length(type) == 1)

    function(x) {
        do.call(
            paste("proto_impact", type, sep = "_"),
            list(x, ...)
        )
    }
}

# PROTOTYPE of BINARY util functions
proto_util_binary <<- function(vec) {
    stopifnot(is.numeric(vec))

    as.numeric(any(vec >= 1))
}

# PROTOTYPE of MAX util functions
proto_util_max <<- function(vec) {
    stopifnot(is.numeric(vec))

    max(vec)
}

# PROTOTYPE of SUM util functions
proto_util_sum <<- function(vec) {
    stopifnot(is.numeric(vec))

    sum(vec)
}

# PROTOTYPE of LOG of sum util functions
proto_util_log_sum <<- function(vec) {
    stopifnot(is.numeric(vec))

    log(sum(vec) + 1)
}

# general GETTER of util functions
get_util_f_type <<- function(type, ...) {
    stopifnot(is.character(type))
    stopifnot(length(type) == 1)

    function(x) {
        do.call(
            paste("proto_util", type, sep = "_"),
            list(x, ...)
        )
    }
}

} # ENDIF
