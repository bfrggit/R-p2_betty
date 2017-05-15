library(dplyr)

get_stats_kth_walkers_per <<- function(df) {
    summarise(
        group_by(df, id),
        t_len = max(t) - min(t),
        x_min = min(x),
        x_max = max(x),
        y_min = min(y),
        y_max = max(y)
    ) # RETURN
}

get_stats_kth_walkers_all <<- function(df) {
    summarise(
        df,
        t_min = min(t_len),
        t_max = max(t_len),
        x_min = min(x_min),
        x_max = max(x_max),
        y_min = min(y_min),
        y_max = max(y_max)
    ) # RETURN
}
