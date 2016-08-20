#Stochastic implementation of 3-d transport system simulation

#P = n-dimensional transition matrix
#s_0 = n by 1 column vector of population in whole numbered values
#r_0 = rate of logistic growth
#t = amount of time to run simulation for
#N = number of individuals in initial population
#b = birth rate
#d = death rate
#v = movement rate

stochastic_sim <- function(P, b, d, s_0, t, N, v) {
    results <- list()
    event <- 0
    time = 0
    s <- N * (s_0 / sum(s_0)) 
    results[[event + 1]] <- c(s, time)
    next_rec = 1
    
        e(time < t) {
        #Generate timestep to next event
        rnum <- runif(1)
        
        
        r_move <- sum(v * s)
        r_birth <- sum(b * s)
        r_death <- sum(b/(N) * s**2)
        r_total <- r_move + r_birth + r_death
        r <- rnum * r_total
        timestep <- (1 / r_total*(log(1 / rnum)))
        
        
        #Logistic growth during timestep. Growth is bounded to N.
        
        #Determine whether birth / death or movement occurs
             < r_move) {
            rand <- runif(1)
            rand <- rand * sum(s)
                and < s[1]) {
                #Moves out from population 1
                rates <- P[,1]
                rand <- runif(1)
                    and < rates[1]) {
                    #Nothing happens because 1 transfers to self
                }
                     if(rand < rates[1] + rates[2]) {
                    #1 transfers to 2
                    s[2] <- s[2] + 1
                    s[1] <- s[1] - 1
                }
                     {
                    #1 transfers to 3
                    s[3] <- s[3] + 1
                    s[1] <- s[1] - 1                
                }
            }
                 if(rand < s[1] + s[2]) {
                #Moves out from population 2
                rates <- P[,2]
                rand <- runif(1)
                    and < rates[1]) {
                    s[1] <- s[1] + 1
                    s[2] <- s[2] - 1
                }
                     if(rand < rates[1] + rates[2]) {
                    #Nothing happens because 2 transfers to self
                }
                     {
                    #2 transfers to 3
                    s[3] <- s[3] + 1
                    s[2] <- s[2] - 1
                }
            }
                 {
                #Moves out from population 3
                rates <- P[,3]
                rand <- runif(1)
                    and < rates[1]) {
                    #3 transfers to 1
                    s[1] <- s[1] + 1
                    s[3] <- s[3] - 1
                }
                     if(rand < rates[1] + rates[2]) {
                    #3 transfers to 2
                    s[2] <- s[2] + 1
                    s[3] <- s[3] - 1
                }
                     {
                    #Nothing happens because 3 transfers to self
                }
            }
        }
        
             {
            rand <- runif(1)
            rand <- rand * sum(r_birth, r_death)
                and < r_death) {
                rand <- runif(1)
                rates <- b/(N) * s^2
                rand <- sum(rates) * rand
                    and < rates[1]) {
                    s[1] = s[1] - 1
                }
                     if(rand < rates[1] + rates[2]) {
                    s[2] = s[2] - 1
                }
                     {
                    s[3] = s[3] - 1
                }
            }
                 {
                rand <- runif(1)
                rates <- s
                rand <- sum(rates) * rand
                    and < rates[1]) {
                    s[1] = s[1] + 1
                }
                     if(rand < rates[1] + rates[2]){
                    s[2] = s[2] + 1
                }
                     {
                    s[3] = s[3] + 1
                }
            }
        }
        
        #Determine where the individual moves out of at the event
        
        
        #Update everything
        time = time + timestep
        event = event + 1 
        
        #Uses event to keep data frame formatted properly, but don't record it.
        
            ime > next_rec) {
            results[[next_rec+1]] <- c(floor(time), s)
            next_rec = next_rec + 1
        }
        
    }
                          matrix(unlist(results), nrow=length(results), byrow=T),
                          stringsAsFactors=FALSE)
    names(results) <- c("Time", "N1", "N2", "N3")
    results
}
    

system.time(stochastic <- stochastic_sim(P, b, d, s_0, max(times), N, v))
#par(mfrow = c(2,1))
#plot(results$N1, results$time)
#plot(out$N1, out$time)
