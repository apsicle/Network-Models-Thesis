#Stochastic implementation of 3-d transport system simulation

#P = n-dimensional transition matrix
#s_0 = n by 1 column vector of population in whole numbered values
#r_0 = rate of logistic growth
#t = amount of time to run simulation for
#K = number of individuals in initial population
#b = birth rate
#d = death rate
#v = movement rate

stochastic_sim <- function(P, b, d, s, t, K, v) {
    dimension <- length(s)
    results <- list()
    event <- 0
    time = 0 
    results[[event + 1]] <- c(time, s)
    next_rec = 1
    
    while(time < t) {
        #Generate timestep to next event
        rnum <- runif(1)
        
        r_move <- sum(s)
        r_birth <- sum(b * s)
        r_death <- sum(b/(K) * s**2)
        r_total <- r_move + r_birth + r_death
        r <- rnum * r_total
        timestep <- ((1 / r_total)*(log(1 / rnum)))
        
        #Logistic growth during timestep. Growth is bounded to K.
        
        #Determine whether birth / death or movement occurs
        if(r < r_move) {
            rand <- runif(1)
            rand <- rand * sum(s)
            
            src <- 1
            found_src = FALSE
            
            while(src <= dimension & found_src == FALSE) {
                
                if(rand <= sum(s[1:src])) {
                    #Stop once source is found
                    rates <- P[,src]
                    rand <- runif(1)
                    dest <- 1
                    found_src = TRUE
                    found_dest = FALSE
                    
                    while(dest <= dimension & found_dest == FALSE) {
                        
                        if(rand < sum(rates[1:dest])) {
                            s[src] <- s[src] - 1
                            s[dest] <- s[dest] + 1
                            found_dest = TRUE
                        }
                        dest = dest + 1       #If not found, increment
                    }
                }
                src = src + 1                #If not found, increment
            }
            
        } else {
            rand <- runif(1)
            rand <- rand * sum(r_birth, r_death)
            if(rand < r_death) {        #Choose birth or death
                rand <- runif(1)
                rates <- b/(K) * s^2
                rand <- sum(rates) * rand
                
                patch <- 1
                found_patch = FALSE
                
                while(patch <= dimension & found_patch == FALSE) {       #Find patch of death
                    
                    if(rand < sum(rates[1:patch])) {
                        s[patch] = s[patch] - 1
                        found_patch = TRUE
                    }
                    patch = patch + 1
                }
                
            } else {                      #If not chosen death, then birth.
                rand <- runif(1)
                rates <- s
                rand <- sum(rates) * rand       
                
                patch <- 1
                found_patch = FALSE
                
                while(patch <= dimension & found_patch == FALSE) {       #Find patch of birth
                    
                    if(rand < sum(rates[1:patch])) {
                        s[patch] = s[patch] + 1
                        found_patch = TRUE
                    }
                    patch = patch + 1
                }
            }
        }
        
        #Update everything
        time = time + timestep
        event = event + 1 
        
        #Uses event to keep data frame formatted properly, but don't record it.
        
        if(time > next_rec) {
            results[[next_rec+1]] <- c(floor(time), s)
            next_rec = next_rec + 1
        }
        
    }
    results <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T),
                          stringsAsFactors=FALSE)
    patch_names <- paste("N", 1:dimension, sep = "")
    names(results) <- c("Time", patch_names)
    results
}


#system.time(stochastic <- stochastic_sim(P, b, d, s_0, max(times), K, v))
#par(mfrow = c(2,1))
#plot(results$N1, results$time)
#plot(out$N1, out$time)
