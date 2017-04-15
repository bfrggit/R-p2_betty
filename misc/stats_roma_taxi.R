library(dplyr)

get_stats_roma_taxi_per <<- function(df) {
    summarise(
        group_by(df, id),
        t_len   = max(t) - min(t),
        lat_min = min(lat),
        lat_max = max(lat),
        lon_min = min(lon),
        lon_max = max(lon)
    ) # RETURN
}

get_stats_roma_taxi_all <<- function(df) {
    summarise(
        df,
        t_min   = min(t_len),
        t_max   = max(t_len),
        lat_min = min(lat_min),
        lat_max = max(lat_max),
        lon_min = min(lon_min),
        lon_max = max(lon_max)
    ) # RETURN
}
