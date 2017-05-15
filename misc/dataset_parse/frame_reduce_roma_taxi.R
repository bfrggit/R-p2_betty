rm(list = ls())

library("dplyr")

get_frame_reduce_id = function(df, lat_min, lat_max, lon_min, lon_max) {
    unique(
        filter(
            df,
            lat < lat_min | lat > lat_max | lon < lon_min | lon > lon_max
        )[, "id"]
    ) # RETURN
}

for(day in 1:30) {
    load(sprintf("taxi_february_%02d.RData", day))
    df = eval(parse(text = sprintf("taxi_february_%02d", day)))
    do.call(
        "<<-",
        list(
            sprintf("taxi_february_%02d", day),
            filter(
                df,
                ! id %in% get_frame_reduce_id(
                    df,
                    lat_min = 41.75,
                    lat_max = 42.05,
                    lon_min = 12.3,
                    lon_max = 12.7
                )
            )
        )
    )
    rm(df)
    save.image(file = sprintf("taxi_february_%02d_reduced.RData", day))
    eval(parse(text = sprintf("rm(\"taxi_february_%02d\")", day)))
}
