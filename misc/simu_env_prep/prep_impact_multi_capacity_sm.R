# prep_impact_multi_capacity_sm.R
#
# Created: 2017-5-14
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

# general simulation SETTINGS
num_nodes = 1800L
num_static = 1200L
num_mob = num_nodes - num_static

# mode-specific simulation SETTINGS
val_k_gas   = 6L
val_k_audio = 1L
val_k_photo = 2L
val_k_wifi  = 1L
lockBinding("val_k_gas", globalenv())
lockBinding("val_k_audio", globalenv())
lockBinding("val_k_photo", globalenv())
lockBinding("val_k_wifi", globalenv())

num_types = 10L

# mode-specific simulation SETTINGS
num_gas_mob = 4L
lockBinding("num_gas_mob", globalenv())

p_audio_mob = c(audio_1 = 0.6)
p_photo_mob = c(photo_1 = 0.9, photo_2 = 0.3)
p_photo_static = c(photo_1 = 0, photo_2 = 0.7)
lockBinding("p_audio_mob", globalenv())
lockBinding("p_photo_mob", globalenv())
lockBinding("p_photo_static", globalenv())

source("lib/element_multi.R")

# CREATE test case elements
rows_mobile = 1L:num_mob
rows_static = (num_mob + 1L):num_nodes

seeds = 1L:10L
for(seed in seeds) {
    cat(
        sprintf("Make random capacity seed = %d\n", seed)
    )
    set.seed(seed)
    capacity_mat = get_capacity_mat_multi(
        val_n           = num_nodes,
        num_static      = num_static,
        val_k_gas       = val_k_gas,
        val_k_audio     = val_k_audio,
        val_k_photo     = val_k_photo,
        val_k_wifi      = val_k_wifi,
        num_gas_mob     = num_gas_mob,
        p_audio_mob     = p_audio_mob,
        p_photo_mob     = p_photo_mob,
        p_photo_static  = p_photo_static
    )
    capacity_mobile = capacity_mat[rows_mobile, ]
    capacity_static = capacity_mat[rows_static, ]

    save(
        capacity_mobile, capacity_static,
        num_gas_mob,
        p_audio_mob, p_photo_mob, p_photo_static,
        file = sprintf("prep_RData/impact_multi_capacity_sm_%d.RData", seed)
    )
}
