# placement_rand.R
#
# Author: Charles Zhu

if(!exists("EX_PLACEMENT_RAND_R")) {
    EX_PLACEMENT_RAND_R <<- TRUE

    source("lib/basic.R")

create_placement_rand <<- function(
    t_frame,
    duration,
    val_n,
    val_m,
    val_k
) {
    # CHECK ARGUMENT TYPES
    stopifnot(is.integer(t_frame))
    stopifnot(is.integer(duration))
    stopifnot(t_frame > 0)
    stopifnot(duration > 0)

    stopifnot(is.integer(val_n))
    stopifnot(is.integer(val_m))
    stopifnot(is.integer(val_k))
    stopifnot(val_n > 0)
    stopifnot(val_m > 0)
    stopifnot(val_k > 0)

    duration_frames = duration %/% t_frame
    stopifnot(is.integer(duration_frames))

    placement_rand_roll <<- array(
        0,
        dim = c(val_n, val_m, duration_frames + 1L)
    )
    dimnames(placement_rand_roll)[[1]] <<- z_nd_str("n", val_n)
    dimnames(placement_rand_roll)[[2]] <<- z_nd_str("c", val_m)
    dimnames(placement_rand_roll)[[3]] <<- z_cl_str("frame", 0L:duration_frames)

    stopifnot(val_n == dim(placement_rand_roll)[1])
    stopifnot(val_m == dim(placement_rand_roll)[2])
    stopifnot(duration_frames + 1L == dim(placement_rand_roll)[3])

    placement_rand_roll # RETURN
}

update_placement_rand <<- function(t_frame, simu_n) {
    val_n = dim(placement_rand_roll)[1]
    val_m = dim(placement_rand_roll)[2]
    duration_frames = dim(placement_rand_roll)[3]

    stopifnot(simu_n >= 0 && simu_n <= duration_frames)

    placement_per_node = as.integer(runif(val_n, min = 1, max = val_m + 1))
    for(jnd in 1:val_n) {
        placement_rand_roll[jnd, placement_per_node[jnd], simu_n + 1L] <<- 1
    }

    placement_rand_roll # RETURN
}

get_placement_rand <<- function() {
    placement_rand_roll # RETURN
}

} # ENDIF
