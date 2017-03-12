getPlacementMatrixRandom = function(valN, valM) {
    stopifnot(valN == as.integer(valN))
    stopifnot(valM == as.integer(valM))
    stopifnot(valN > 0)
    stopifnot(valM > 0)
    matG = matrix(
        0,
        nrow = valN, ncol = valM, byrow = TRUE
    )
    placementPerNode = as.integer(runif(valN, min = 1, max = valM + 1))
    for(jnd in 1:valN) {
        matG[jnd, placementPerNode[jnd]] = 1
    }
    rownames(matG) = paste("n", as.character(1:valN), sep = "_")
    colnames(matG) = paste("c", as.character(1:valM), sep = "_")
    matG
}