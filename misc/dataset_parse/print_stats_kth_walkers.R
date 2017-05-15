rm(list = ls())

source("stats_kth_walkers.R")

for(w in c(30, 40, 50, 70, 90)) {
    load(sprintf("ostermalm_%03d_2.RData", w))
    df = eval(parse(text = sprintf("ostermalm_%03d_2", w)))
    cat(sprintf("ostermalm_%03d_2.RData", w), "\n")
    print(get_stats_kth_walkers_all(get_stats_kth_walkers_per(df)))
    cat("\n")
}
rm(df)
