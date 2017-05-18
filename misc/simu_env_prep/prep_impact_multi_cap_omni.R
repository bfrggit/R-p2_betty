# prep_impact_multi_capacity_omni.R
#
# Created: 2017-5-17
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

# general simulation SETTINGS
num_nodes = 1800L
# num_static = 1200L
# num_mob = num_nodes - num_static

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
num_gas = 4L
lockBinding("num_gas", globalenv())

p_audio = c(audio_1 = 0.84)
p_photo = c(photo_1 = 0.36, photo_2 = 0.54)
lockBinding("p_audio", globalenv())
lockBinding("p_photo", globalenv())

source("lib/element_multi.R")

seeds = 1L:10L
for(seed in seeds) {
    cat(
        sprintf("Make random capacity seed = %d\n", seed)
    )
    set.seed(seed)
    capacity_mat = get_capacity_mat_multi_omni(
        val_n       = num_nodes,
        val_k_gas   = val_k_gas,
        val_k_audio = val_k_audio,
        val_k_photo = val_k_photo,
        val_k_wifi  = val_k_wifi,
        num_gas     = num_gas,
        p_audio     = p_audio,
        p_photo     = p_photo
    )
    capacity_omni = capacity_mat

    save(
        capacity_omni,
        num_gas,
        p_audio, p_photo,
        file = sprintf("prep_RData/impact_multi_cap_omni_%d.RData", seed)
    )
}
