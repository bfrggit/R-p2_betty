# basic.R
#
# Author: Charles Zhu

if(!exists("EX_BASIC_R")) {
    EX_BASIC_R <<- TRUE

# RETURN z_ notation strings
z_nd_str <<- function(str_z, val_n) {
    stopifnot(is.character(str_z))
    stopifnot(is.integer(val_n))
    stopifnot(val_n > 0)
    stopifnot(length(str_z) == 1)

    paste(str_z, as.character(1:val_n), sep = "_") # RETURN
}

} # ENDIF
