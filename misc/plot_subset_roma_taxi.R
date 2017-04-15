rm(list = ls())

for(day in 1:30) {
    load(sprintf("taxi_february_%02d.RData", day))
    df = eval(parse(text = sprintf("taxi_february_%02d", day)))
    items = as.integer(
        runif(
            n = 60000,
            min = 0,
            max = nrow(df)
        )
    ) + 1L

    # prepare and plot as PNG file
    png = png(
        filename = sprintf("img/taxi_february_%02d_subset.png", day),
        width = 8,
        height = 7.27,
        units = "in",
        res = 300
    )
    plot(
        df[items, "lon"],
        df[items, "lat"],
        pch = ".",
        main = sprintf("taxi_february_%02d", day),
        xlab = "lon / deg",
        ylab = "lat / deg"
    )
    dev.off

    # prepare and plot as EPS file
    setEPS()
    postscript(
        file = sprintf("img/taxi_february_%02d_subset.eps", day),
        width = 8,
        height = 7.27,
        title = sprintf("taxi_february_%02d", day),
        onefile = FALSE
    )
    plot(
        df[items, "lon"],
        df[items, "lat"],
        pch = ".",
        main = sprintf("taxi_february_%02d", day),
        xlab = "lon / deg",
        ylab = "lat / deg"
    )
    dev.off
}
rm(df)
rm(items)
