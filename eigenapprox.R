#Eigenvalue approximation
A <- P[2:3, 2:3]
ev <- eigen(A)

values <- ev$values
vectors <- ev$vectors
sums <- colSums(vectors)
max_vec <- vectors[, which(sums == max(sums))]
max_val <- max(values)

#Note that s_minus = state vector of 1 dimension lower than s (ie. minus the first patch / row)

x_t <- function(val = max_val, vec = max_vec) {
    results <- list()
    time = 0
    s <- N * (s_0 / sum(s_0)) 
    results[[time + 1]] <- c(s, time)
    for(i in 1:max(times)) {
        #s_minus <- s[-1]
        #s <- P[-1,1] + A*s_minus
        s_minus <- (max_val^(i-1)) * max_vec
        s <- s + c(0, s_minus)
        time = time + 1
        results[[time + 1]] <- c(s, time)
    }
    results <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T),
                          stringsAsFactors=FALSE)
    names(results) <- (c("N1", "N2", "N3", "Time"))
    results
}

add_uneven <- function(x, y) {
    len <- max(length(x), length(y))
    length(x) <- len
    length(y) <- len
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0
    x + y
}