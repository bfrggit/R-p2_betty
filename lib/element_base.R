# element_base.R
#
# Created: As part of the initial version of the project
# Updated: 2017-4-30
#  Author: Charles Zhu
#
if(!exists("EX_ELEMENT_BASE_R")) {
    EX_ELEMENT_BASE_R <<- TRUE

    source("lib/basic.R")

# GENERATE LOCAL ONLY impact functions
# make_impact_f_local_only <<- function(val_k) {
#     stopifnot(is.integer(val_k))
#     stopifnot(length(val_k) == 1L)
#     stopifnot(val_k > 0L)
#
#     for(knd in 1L:val_k) {
#         do.call(
#             "<<-", list(
#                 paste("s_impact_f", knd, sep = "_"),
#                 get_impact_f_type("local_only")
#             )
#         )
#         do.call(
#             "<<-", list(
#                 paste("t_impact_f", knd, sep = "_"),
#                 get_impact_f_type("local_only")
#             )
#         )
#     }
#
#     NA # NO RETURN
# }

# GENERATE impact functions using general getters for selected data types
make_s_impact_f_type_vec <<- function(type, kcl, ...) {
    stopifnot(is.character(type))
    stopifnot(length(type) == 1)
    stopifnot(is.integer(kcl))
    stopifnot(length(kcl) > 0L)
    stopifnot(kcl > 0L)

    for(knd in kcl) {
        do.call(
            "<<-", list(
                paste("s_impact_f", knd, sep = "_"),
                get_impact_f_type(type, ...)
            )
        )
    }

    NA # NO RETURN
}

make_t_impact_f_type_vec <<- function(type, kcl, ...) {
    stopifnot(is.character(type))
    stopifnot(length(type) == 1)
    stopifnot(is.integer(kcl))
    stopifnot(length(kcl) > 0L)
    stopifnot(kcl > 0L)

    for(knd in kcl) {
        do.call(
            "<<-", list(
                paste("t_impact_f", knd, sep = "_"),
                get_impact_f_type(type, ...)
            )
        )
    }

    NA # NO RETURN
}

# GENERATE impact functions using general getters
# make_s_impact_f_type <<- function(type, val_k, ...) {
#     stopifnot(is.character(type))
#     stopifnot(length(type) == 1)
#     stopifnot(is.integer(val_k))
#     stopifnot(length(val_k) == 1L)
#     stopifnot(val_k > 0L)
#
#     for(knd in 1L:val_k) {
#         do.call(
#             "<<-", list(
#                 paste("s_impact_f", knd, sep = "_"),
#                 get_impact_f_type(type, ...)
#             )
#         )
#     }
#
#     NA # NO RETURN
# }
#
# make_t_impact_f_type <<- function(type, val_k, ...) {
#     stopifnot(is.character(type))
#     stopifnot(length(type) == 1)
#     stopifnot(is.integer(val_k))
#     stopifnot(length(val_k) == 1L)
#     stopifnot(val_k > 0L)
#
#     for(knd in 1L:val_k) {
#         do.call(
#             "<<-", list(
#                 paste("t_impact_f", knd, sep = "_"),
#                 get_impact_f_type(type, ...)
#             )
#         )
#     }
#
#     NA # NO RETURN
# }

make_s_impact_f_type <<- function(type, val_k, ...) {
    stopifnot(is.integer(val_k))
    stopifnot(length(val_k) == 1L)
    stopifnot(val_k > 0L)

    make_s_impact_f_type_vec(type, 1L:val_k, ...)
}

make_t_impact_f_type <<- function(type, val_k, ...) {
    stopifnot(is.integer(val_k))
    stopifnot(length(val_k) == 1L)
    stopifnot(val_k > 0L)

    make_t_impact_f_type_vec(type, 1L:val_k, ...)
}

# GENERATE LOCAL ONLY impact functions
make_impact_f_local_only <<- function(val_k) {
    stopifnot(is.integer(val_k))
    stopifnot(length(val_k) == 1L)
    stopifnot(val_k > 0L)

    make_s_impact_f_type(type = "local_only", val_k = val_k)
    make_t_impact_f_type(type = "local_only", val_k = val_k)
}

} # ENDIF
