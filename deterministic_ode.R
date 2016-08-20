require(deSolve)
#First two terms from r(x)(1- (x/K)). Note that b = r and d = r/K


#state <- c(N1 = 5000, N2 = 0, N3 = 0)
#times <- seq(0, 100)

#In this case, b = 0.05 and d = 0.05 since d = b / K and K = 1
deterministic_ode_solve <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        #rates of change
        names <- paste("dN", (1:dimensions), sep = "")
        results <- vector()
        vals <- rep(NA, dimensions)
        
        for(i in 1:dimensions) {
            vals[i] <- ((b * state[i]) - (d / K * state[i]^2) 
                        - (sum(P[-i, i] * state[i])) + (sum(P[i, -i] * state[-i])))
        }
        
        for(i in 1:dimensions) {
            assign(names[i], 
                   vals[i])
            
            #assign(names[i], 
             #      ((- (sum(P[-i, i] * state[i])) + (sum(P[i, -i] * state[-i])))))
            #assign(names[i], 
                         #((b * state[i]) - (d / (2*K) * state[i]^2)))
            
            results[i] <- get(names[i])
        }
        list(results)
    })
}

#out <- ode(y = state, times = times, func = deterministic_ode_solve, parms = parameters)
#out <- data.frame(out)
#names(out)[-1] <- 1:parameters$dimension
#test <- melt(out, id.vars = "time", variable.name = "patch", value.name = "population")
#View(out)
