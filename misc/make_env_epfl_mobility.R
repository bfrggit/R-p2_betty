rm(list = ls())

load("_cabs.RData")
library("dplyr")

for(id in cabs_meta[, 1]) {
    df = read.table(sprintf("new_%s.txt", id), sep = " ", header = FALSE)
    colnames(df) = c("lat", "lon", "fare", "t")
    df = arrange(df, t)
    do.call(
        "<<-",
        list(
            sprintf("cabspottingdata_%s", id),
            df[, c("t", "lat", "lon", "fare")]
        )
    )
}
rm(df)
