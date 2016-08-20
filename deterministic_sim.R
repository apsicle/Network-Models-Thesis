#Simple 2-d transport system simulation

simulate <- function(P, v, s, r_0 = b, t = max(times)) {
    results <- list()
    K <- sum(s)
    results[[1]] <- s
    for(i in 1:t) {
        s <- (exp(r_0)*s) / (1 + s*((exp(r_0) - 1)/K))
        s <- P %*% s
        results[[i+1]] <- s
    }
    results <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T),
                          stringsAsFactors=FALSE)
    dimension <- length(s)
    names <- paste("N", (1:dimension), sep = "")
    names(results) <- (names)
    results <- cbind("time" = 0:t, results)
    results
}

#y <- simulate(P, v, state, t = max(times))
#deterministic <- data.frame(Time = times, y)
#View(deterministic)
