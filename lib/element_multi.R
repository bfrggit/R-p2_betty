# element_multi.R
#
# Created: 2017-4-23
#  Author: Charles Zhu
#
if(!exists("EX_ELEMENT_MULTI_R")) {
    EX_ELEMENT_MULTI_R <<- TRUE

    source("lib/basic.R")

data_rate_multi_0_by_type = c(  # data rate of several data types, byte / sample
    gas     = 200,              # experimental data, JSON text, byte / sample
    audio   = 800000,           # experimental data, WAV, byte / sample (8 sec)
    # bmp_1   = 15116544,       # at 2592x1944, 24 bit, byte / sample
    # bmp_2   = 24245760,       # at 3280x2464, 24 bit, byte / sample
    # jpg_1   = 10354832,       # estimation for photos, 5 MP, byte / sample
    # jpg_2   = 16608345,       # estimation for photos, 8 MP, byte / sample
    jpg     = 180000,           # experimental data, at 720x480, byte / sample
    wifi    = 3600              # at 180 byte per AP, 20 APs, byte / sample
)
# JPEG compression ratio for photos (ref)
#   0.315 at quality = 70
#   http://stackoverflow.com/questions/3471663/jpeg-compression-ratio

data_rate_multi_by_type = data_rate_multi_0_by_type / c( # sec / sample
    gas     = 5,
    audio   = 7.5,              # 60 / this_value = length_of_sample / min
    # bmp_1   = 20,
    # bmp_2   = 20,
    # jpg_1   = 20,
    # jpg_2   = 20,
    jpg     = 20,
    wifi    = 4
) # order of types MUST MATCH with original rates

# GENERATE sensing capacity matrix for all nodes
# for mobile nodes
#   fixed num of gas sensors are chosen from all candidates
#   audio and photo sensors present with probability
#   all Wi-Fi sensors present
# for static nodes
#   all gas sensors are chosen
#   all audio sensors present
#   photo sensors present with probability
#   all Wi-Fi sensors present
get_capacity_mat_multi <<- function(
    val_n,
    num_static,
    val_k_gas,
    val_k_audio,
    val_k_photo,
    val_k_wifi = 1L,            # should be ONE in a normal scenario
    num_gas_mob,                # num gas sensors per mobile node
    p_audio_mob,                # probability that each audio sensor presents
    p_photo_mob,                # probability that each photo sensor presents
    p_photo_static              # probability that each photo sensor presents
) {
    stopifnot(is.integer(val_n))
    stopifnot(length(val_n) == 1L)
    stopifnot(val_n > 0L)

    stopifnot(is.integer(num_static))
    stopifnot(length(num_static) == 1L)
    stopifnot(num_static <= val_n)
    stopifnot(num_static >= 0L)

    num_mob = val_n - num_static
    rows_mobile = 1L:num_mob
    rows_static = (num_mob + 1L):val_n

    stopifnot(is.integer(val_k_gas))
    stopifnot(is.integer(val_k_audio))
    stopifnot(is.integer(val_k_photo))
    stopifnot(is.integer(val_k_wifi))
    stopifnot(length(val_k_gas) == 1L)
    stopifnot(length(val_k_audio) == 1L)
    stopifnot(length(val_k_photo) == 1L)
    stopifnot(length(val_k_wifi) == 1L)
    stopifnot(val_k_gas >= 0L)
    stopifnot(val_k_audio >= 0L)
    stopifnot(val_k_photo >= 0L)
    stopifnot(val_k_wifi > 0L)
    stopifnot(val_k_gas + val_k_audio + val_k_photo > 0L)

    val_k = val_k_gas + val_k_audio + val_k_photo + val_k_wifi
    col_type_lt = cumsum(c(1L, val_k_gas, val_k_audio, val_k_photo))
    col_type_rt = cumsum(c(val_k_gas, val_k_audio, val_k_photo, val_k_wifi))
    names(col_type_lt) = c("gas", "audio", "photo", "wifi")
    names(col_type_rt) = c("gas", "audio", "photo", "wifi")

    # stopifnot(is.numeric(p))    # p is the probability of ONE assignments
    # stopifnot(length(p) == 1L)
    # stopifnot(p > 0 && p <= 1)

    stopifnot(is.integer(num_gas_mob))
    stopifnot(length(num_gas_mob) == 1L)
    stopifnot(num_gas_mob <= val_k_gas)
    stopifnot(num_gas_mob >= 0L)

    stopifnot(is.numeric(p_audio_mob))
    stopifnot(is.numeric(p_photo_mob))
    stopifnot(is.numeric(p_photo_static))
    stopifnot(length(p_audio_mob) == val_k_audio)
    stopifnot(length(p_photo_mob) == val_k_photo)
    stopifnot(length(p_photo_static) == val_k_photo)
    stopifnot(p_audio_mob >= 0)
    stopifnot(p_audio_mob <= 1)
    stopifnot(p_photo_mob >= 0)
    stopifnot(p_photo_mob <= 1)
    stopifnot(p_photo_static >= 0)
    stopifnot(p_photo_static <= 1)

    mat_c = matrix(
        0,
        nrow = val_n,
        ncol = val_k,
        byrow = TRUE
    )
    rownames(mat_c) = z_nd_str("n", val_n)
    colnames(mat_c) = z_nd_str("d", val_k)
    # c_t = runif(val_n * val_k, min = 0, max = 1)
    # c_p = ifelse(c_t > p, 0, 1)
    # mat_c[] = c_p

    # SPAWN mobile nodes
    for(jnd in rows_mobile) {
        mat_c[
            jnd, col_type_lt["gas"] + sample(
                1L:val_k_gas - 1L,
                num_gas_mob
            )
        ] = 1
    }
    mat_c[rows_mobile, col_type_lt["audio"]:col_type_rt["audio"]] = matrix(
        ifelse(
            runif(num_mob * val_k_audio, min = 0, max = 1) > p_audio_mob,
            0, 1
        ), ncol = val_k_audio, byrow = TRUE
    )
    mat_c[rows_mobile, col_type_lt["photo"]:col_type_rt["photo"]] = matrix(
        ifelse(
            runif(num_mob * val_k_photo, min = 0, max = 1) > p_photo_mob,
            0, 1
        ), ncol = val_k_photo, byrow = TRUE
    )

    # SPAWN static nodes
    mat_c[rows_static, col_type_lt["gas"]:col_type_rt["gas"]] = 1
    mat_c[rows_static, col_type_lt["audio"]:col_type_rt["audio"]] = 1
    mat_c[rows_static, col_type_lt["photo"]:col_type_rt["photo"]] = matrix(
        ifelse(
            runif(num_static * val_k_photo, min = 0, max = 1) > p_photo_static,
            0, 1
        ), ncol = val_k_photo, byrow = TRUE
    )

    # all Wi-Fi sensors present on all nodes
    mat_c[, col_type_lt["wifi"]:col_type_rt["wifi"]] = 1

    mat_c # RETURN
}

# GENERATE data type specs using num sensors of each type plus weights
get_data_type_spec_df_multi <<- function(
    val_k_gas,
    val_k_audio,
    val_k_photo,
    val_k_wifi = 1L,    # should be ONE in a normal scenario
    weight_original
) {
    # stopifnot(is.integer(val_k))
    # stopifnot(length(val_k) == 1L)
    # stopifnot(val_k > 0L)

    stopifnot(is.integer(val_k_gas))
    stopifnot(is.integer(val_k_audio))
    stopifnot(is.integer(val_k_photo))
    stopifnot(is.integer(val_k_wifi))
    stopifnot(length(val_k_gas) == 1L)
    stopifnot(length(val_k_audio) == 1L)
    stopifnot(length(val_k_photo) == 1L)
    stopifnot(length(val_k_wifi) == 1L)
    stopifnot(val_k_gas >= 0L)
    stopifnot(val_k_audio >= 0L)
    stopifnot(val_k_photo >= 0L)
    stopifnot(val_k_wifi > 0L)
    stopifnot(val_k_gas + val_k_audio + val_k_photo > 0L)

    val_k = val_k_gas + val_k_audio + val_k_photo + val_k_wifi

    stopifnot(is.numeric(weight_original))
    stopifnot(length(weight_original) == val_k)

    df_d = data.frame(
        rate = c(
            rep(data_rate_multi_by_type["gas"], val_k_gas),
            rep(data_rate_multi_by_type["audio"], val_k_audio),
            rep(data_rate_multi_by_type["jpg"], val_k_photo),
            rep(data_rate_multi_by_type["wifi"], val_k_wifi)
        ),
        weight = weight_original / sum(weight_original),
        s_impact_f = z_nd_str("s_impact_f", val_k),
        t_impact_f = z_nd_str("t_impact_f", val_k),
        row.names = z_nd_str("d", val_k),
        check.names = TRUE,
        fix.empty.names = TRUE,
        stringsAsFactors = FALSE
    )

    df_d # RETURN
}

} # ENDIF
