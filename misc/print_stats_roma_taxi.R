rm(list = ls())

source("stats_roma_taxi.R")

for(day in 1:30) {
    load(sprintf("taxi_february_%02d.RData", day))
    df = eval(parse(text = sprintf("taxi_february_%02d", day)))
    cat(sprintf("taxi_february_%02d.RData", day), "\n")
    print(get_stats_roma_taxi_all(get_stats_roma_taxi_per(df)))
    cat("\n")
}
rm(df)
