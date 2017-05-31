# prep_impact_multi_capacity_dist.R
#
# Created: 2017-5-30
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

# general simulation SETTINGS
num_nodes = seq(50L, 200L, by = 6L)
ref_nodes = 50L

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

# num of sensors for reference
ref_gas = as.integer(ref_nodes * (num_gas / val_k_gas))
ref_audio = as.integer(ref_nodes * p_audio)
ref_photo = as.integer(ref_nodes * p_photo)
ref_wifi = ref_nodes
lockBinding("ref_gas", globalenv())
lockBinding("ref_audio", globalenv())
lockBinding("ref_photo", globalenv())
lockBinding("ref_wifi", globalenv())

# prepare data for fast generation
val_k_cum = cumsum(c(0L, val_k_gas, val_k_audio, val_k_photo, val_k_wifi))

source("lib/basic.R")

cap_zero_max = matrix(0, nrow = num_nodes[length(num_nodes)], ncol = num_types)
rownames(cap_zero_max) = z_nd_str("n", nrow(cap_zero_max))
colnames(cap_zero_max) = z_nd_str("d", num_types)

cap_index_map = 1L:length(num_nodes)
names(cap_index_map) = num_nodes

seeds = 1L:10L
for(seed in seeds) {
    cat(
        sprintf("Make random capacity seed = %d\n", seed)
    )
    set.seed(seed)
    capacity_dist = list()

    # manually generate capacity mat
    # disregarding pre-defined func
    for(rnd in cap_index_map) {
        rnd_nodes = num_nodes[rnd]
        capacity_mat = cap_zero_max[1L:rnd_nodes, ]
        for(tpd in 1L:val_k_gas)
            capacity_mat[
                sample(1L:rnd_nodes, ref_gas),
                tpd
            ] = 1
        for(tpd in 1L:val_k_audio)
            capacity_mat[
                sample(1L:rnd_nodes, ref_audio[tpd]),
                tpd + val_k_cum[2]
            ] = 1
        for(tpd in 1L:val_k_photo)
            capacity_mat[
                sample(1L:rnd_nodes, ref_photo[tpd]),
                tpd + val_k_cum[3]
            ] = 1
        for(tpd in 1L:val_k_wifi)
            capacity_mat[
                sample(1L:rnd_nodes, ref_wifi[tpd]),
                tpd + val_k_cum[4]
            ] = 1
        capacity_dist[[rnd]] = capacity_mat
    }

    save(
        capacity_dist, cap_index_map,
        num_gas,
        p_audio, p_photo,
        ref_nodes, ref_gas, ref_audio, ref_photo, ref_wifi,
        file = sprintf("prep_RData/impact_multi_cap_dist_%d.RData", seed)
    )
}
