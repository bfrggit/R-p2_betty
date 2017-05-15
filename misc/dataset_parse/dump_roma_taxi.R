rm(list = ls())

for(day in 1:30) {
    do.call(
        "<<-",
        list(
            sprintf("taxi_february_%02d", day),
            read.table(
                sprintf("taxi_february_%02d.txt", day),
                header = FALSE,
                sep = " "
            )
        )
    )
    eval(parse(text = sprintf(
        "colnames(taxi_february_%02d) = c(\"id\", \"t\", \"lat\", \"lon\")",
        day
    )))
    save.image(sprintf("taxi_february_%02d.RData", day))
    eval(parse(text = sprintf("rm(\"taxi_february_%02d\")", day)))
}
