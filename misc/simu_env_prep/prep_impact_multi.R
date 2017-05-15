# prep_impact_multi.R
#
# Created: 2017-5-14
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

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

weight_types = c(
    c(      # gas
        8,
        5,
        5,
        4,
        4,
        3
    ), c(   # audio
        5
    ), c(   # photo
        7,
        6
    ), c(   # wifi
        3
    )
)
names(weight_types) = paste("d", 1L:num_types, sep = "_")
lockBinding("weight_types", globalenv())

get_t_imp_f = function(t_const) {as.integer(ceiling(t_const * 60 / t_frame))}
col_type_lt = cumsum(c(1L, val_k_gas, val_k_audio, val_k_photo))
col_type_rt = cumsum(c(val_k_gas, val_k_audio, val_k_photo, val_k_wifi))

source("lib/element_base.R")
source("lib/element_multi.R")

data_type_spec_df = get_data_type_spec_df_multi(
    val_k_gas       = val_k_gas,
    val_k_audio     = val_k_audio,
    val_k_photo     = val_k_photo,
    val_k_wifi      = val_k_wifi,
    weight_original = weight_types
)

make_s_impact_f_type_vec(kcl = col_type_lt[1]:col_type_rt[1],
                         type = "exp", t_const = 550)
make_s_impact_f_type_vec(kcl = col_type_lt[2]:col_type_rt[2],
                         type = "exp", t_const = 100)
make_s_impact_f_type_vec(kcl = col_type_lt[3],
                         type = "exp", t_const = 10)
make_s_impact_f_type_vec(kcl = col_type_lt[3] + 1L,
                         type = "exp", t_const = 750)
make_s_impact_f_type_vec(kcl = col_type_lt[4]:col_type_rt[4],
                         type = "exp", t_const = 20)
make_t_impact_f_type_vec(kcl = col_type_lt[1]:col_type_rt[1],
                         type = "step", step = get_t_imp_f(20))
make_t_impact_f_type_vec(kcl = col_type_lt[2]:col_type_rt[2],
                         type = "step", step = get_t_imp_f(5))
make_t_impact_f_type_vec(kcl = col_type_lt[3],
                         type = "step", step = get_t_imp_f(60))
make_t_impact_f_type_vec(kcl = col_type_lt[3] + 1L,
                         type = "step", step = get_t_imp_f(30))
make_t_impact_f_type_vec(kcl = col_type_lt[4]:col_type_rt[4],
                         type = "step", step = get_t_imp_f(60))

save(
    data_type_spec_df,
    num_types,
    val_k_audio, val_k_gas, val_k_photo, val_k_wifi,
    weight_types,
    list = ls(pattern = "[a-z]_impact_f_\\d+"),
    file = "prep_RData/impact_multi.RData"
)
