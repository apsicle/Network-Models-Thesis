#Stochastic implementation of 3-d transport system simulation

#P = n-dimensional transition matrix
#s_0 = n by 1 column vector of population in whole numbered values
#r_0 = rate of logistic growth
#t = amount of time to run simulation for
#N = number of individuals in initial population
#b = birth rate
#d = death rate
#v = movement rate

stochastic_sim <- function(P, s_0, r_0, t, N) {
    results <- list()
    event <- 0
    time = 0
    s <- N * (s_0 / sum(s_0))
    
    while(time < t) {
        #Generate timestep to next event
        rnum <- runif(1)
        timestep <- (1 / sum(s))*(log(1 / rnum))
        r <- sum(s)*rnum
        
        #Logistic growth during timestep. Growth is bounded to N.
     
        #Fractional growth is removed (floor function)
        s <- floor((exp(r_0*timestep)*s) / (1 + s*((exp(r_0*timestep) - 1) / N)))
        
        #See where the individual moves out of at the event
        if(r < s[1]) {
            #Moves out from population 1
            rates <- P[,1]
            rand <- runif(1)
            if(rand < rates[1]) {
                #Nothing happens because 1 transfers to self
            }
            else if(rand < rates[1] + rates[2]) {
                #1 transfers to 2
                s[2] <- s[2] + 1
                s[1] <- s[1] - 1
            }
            else {
                #1 transfers to 3
                s[3] <- s[3] + 1
                s[1] <- s[1] - 1                
            }
        }
        else if(r < s[1] + s[2]) {
            #Moves out from population 2
            rates <- P[,2]
            rand <- runif(1)
            if(rand < rates[1]) {
                s[1] <- s[1] + 1
                s[2] <- s[2] - 1
            }
            else if(rand < rates[1] + rates[2]) {
                #Nothing happens because 2 transfers to self
            }
            else {
                #2 transfers to 3
                s[3] <- s[3] + 1
                s[2] <- s[2] - 1
            }
        }
        else {
            #Moves out from population 3
            rates <- P[,3]
            rand <- runif(1)
            if(rand < rates[1]) {
                #3 transfers to 1
                s[1] <- s[1] + 1
                s[3] <- s[3] - 1
            }
            else if(rand < rates[1] + rates[2]) {
                #3 transfers to 2
                s[2] <- s[2] + 1
                s[3] <- s[3] - 1
            }
            else {
                #Nothing happens because 3 transfers to self
            }
        }
        
        #Update everything
        time = time + timestep
        event = event + 1
        results[[event]] <- c(s, time, event)
    }
    results <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T),
                          stringsAsFactors=FALSE)
    names(results) <- (c("N1", "N2", "N3", "Time", "Step"))
    results
} 
    