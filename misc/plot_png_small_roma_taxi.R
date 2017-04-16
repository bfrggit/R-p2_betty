rm(list = ls())

for(day in 1:30) {
    load(sprintf("taxi_february_%02d_small.RData", day))
    df = eval(parse(text = sprintf("taxi_february_%02d", day)))
    png = png(
        filename = sprintf("img/taxi_february_%02d_small.png", day),
        width = 8,
        height = 7.27,
        units = "in",
        res = 300
    )
    plot(
        df[, "lon"],
        df[, "lat"],
        pch = ".",
        main = sprintf("taxi_february_%02d", day),
        xlab = "lon / deg",
        ylab = "lat / deg"
    )
    dev.off
}
rm(df)
