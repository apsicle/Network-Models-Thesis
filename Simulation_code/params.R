#Generate parameters

#Define global parameters via this function.
gen_matrix <- function(dimensions, v) {
    #Generates a symmetric matrix with 'v' shipped elsewhere.

    vec = rep( c(1 - (dimensions-1)*v, rep(v, dimensions)), dimensions )
    P <<- matrix(vec, ncol = dimensions, nrow = dimensions)
}

param <- function(dimensions, v, b, d, K, times, mat = NA) {
    
    #Default generate matrix: 
    if(is.na(mat)) {
        mat <- gen_matrix(dimensions, v)
    }
    
    #Set parameters for the deterministic ODE model:
    parameters <<- list(b = b, d = d, P = mat, dimensions = dimensions, K = K,
                        v = v)
    
    P <<- mat
    state <<- c(K, rep(0, dimensions -1))
    times <<- seq(0, times)
    
    #Set parameters for the stochastic sim
    v <<- v
    K <<- K
    b <<- b
    d <<- d
}

