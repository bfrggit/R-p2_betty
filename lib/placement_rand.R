# placement_rand.R
#
# Author: Charles Zhu

if(!exists("EX_PLACEMENT_RAND_R")) {
    EX_PLACEMENT_RAND_R <<- TRUE

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

    placement_mat_rand_roll <<- array(
        0,
        dim = c(val_n, val_m, duration_frames)
    )
    stopifnot(val_n == dim(placement_mat_rand_roll)[1])
    stopifnot(val_m == dim(placement_mat_rand_roll)[2])
    stopifnot(duration_frames == dim(placement_mat_rand_roll)[3])

    placement_mat_rand_roll # RETURN
}

update_placement_rand <<- function(t_frame, simu_n) {
    val_n = dim(placement_mat_rand_roll)[1]
    val_m = dim(placement_mat_rand_roll)[2]
    duration_frames = dim(placement_mat_rand_roll)[3]

    stopifnot(simu_n >= 0 && simu_n <= duration_frames)

    placement_mat_rand_roll # RETURN
}

} # ENDIF
