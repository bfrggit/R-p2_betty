rm(list = ls())

library("dplyr")

get_frame_test = function(df, lat_min, lat_max, lon_min, lon_max) {
    id_all = unique(
        df[, "id"]
    )
    id_out = unique(
        filter(
            df,
            lat < lat_min | lat > lat_max | lon < lon_min | lon > lon_max
        )[, "id"]
    )
    num_all = length(id_all)
    rec_all = nrow(df)
    num_out = length(id_out)
    rec_out = nrow(
        filter(
            df,
            id %in% id_out
        )
    )

    data.frame(
        num_all = num_all,
        rec_all = rec_all,
        num_out = num_out,
        rec_out = rec_out,
        num_val = num_all - num_out,
        rec_val = rec_all - rec_out,
        stringsAsFactors = FALSE,
        check.names = TRUE
    ) # RETURN
}

for(day in 1:30) {
    load(sprintf("taxi_february_%02d_reduced.RData", day))
    df = eval(parse(text = sprintf("taxi_february_%02d", day)))
    cat(sprintf("taxi_february_%02d_reduced.RData", day), "\n")
    print(get_frame_test(
        df,
        lat_min = 41.83,    # 41.75
        lat_max = 41.98,    # 42.05
        lon_min = 12.4,     # 12.3
        lon_max = 12.6      # 12.7
    ))
    cat("\n")
}
rm(df)
