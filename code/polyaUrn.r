# dirichlet process via a polya urn

set.seed(42)

observeProcess = function(alpha, bounds, urn) {
    i = 0
    urn[1,] = c(runif(1, min = bounds[1], max = bounds[2]), 
                runif(1, min = bounds[1], max = bounds[2]), i)
    for (j in 2:nrow(urn)) {
        if (runif(1) < (alpha / (j + alpha))) {
            i = i + 1
            urn[j,] = c(runif(1, min = 0, max = 10), runif(1, min = 0, max = 10), i)
        } else {
            urn[j,] = urn[sample(1:(j-1), 1),]
        }  
    }
    return(urn)
}

urn = matrix(nrow = 500, ncol = 3, NA)
urn = observeProcess(1, c(0, 10), urn)
urn = urn[order(urn[,3]),]

plot(jitter(urn, amount = 0.1), pch = 16, xlim = c(0, 10), ylim = c(0, 10))


