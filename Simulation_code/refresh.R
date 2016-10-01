refresh <- function(dim = parameters$dimensions) {
    system.time(stochastic <<- stochastic_sim(P, b, d, s_0, max(times), N, v))
    out <<- ode(y = state, times = times, func = deterministic_ode_solve, parms = parameters)
    if(dim == 2) {
        deterministic_ode <<- data.frame(Time = out[,1], N1 = out[,2], N2 = out[,3])
        p <- make_plots(2)
    }
    else if(dim == 3) {
        deterministic_ode <<- data.frame(Time = out[,1], N1 = out[,2], N2 = out[,3], N3 = out[,3])
        p <- make_plots(3)
    }
    
    p
}

refresh()

go <- function(dimensions, v, b, d, n, times) {
    param(dimensions, v, b, d, n, times)
    refresh()
}
