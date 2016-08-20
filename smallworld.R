# Small-world networks

require(igraph)
require(deSolve)
require(ggplot2)

g <- watts.strogatz.game(dim = 1, size = 50, nei = 2, p = 0.25)
plot(g, layout = layout.circle)

a <- get.adjacency(g, sparse = FALSE) # Produces an undirected unweighted adjacency matrix (symmetric, binary)
a

# Run and aggregate deterministic simulations with multiple rewirings (vary p, aggregate at each p)

run_stats <- function(watt_dim, size, nei, splits, aggregate_runs = 10, move = 0.01, bd = 0.01) {
    opp <- floor(size / 2) + 1    
    aggregate <- rep(0, splits + 1)
    points <- gen_log_points(0.0001,1,splits, 10)
    points <- c(0, points)
    
    for(i in 0:splits) {
        record <- rep(0, aggregate_runs)

        for(j in 1:aggregate_runs) {
        #Run the simulation using the adjacency matrix as P. Repeat # of aggregate_runs
            connected = FALSE
            while(connected == FALSE) {
                g <- watts.strogatz.game(dim = watt_dim, size = size, nei = nei, p = 1-sqrt(1-points[i+1]))
                if(distances(g, v = 1, to = opp) == Inf) {}
                else {connected = TRUE}
            }
            a <- get.adjacency(g, sparse = FALSE)
            
            param(size, move, bd, bd, 5000, 1500, mat = rewrite_adj(a, v= move))
            
            out <- simulate(P = P, v = move, state, r_0 = b, t = max(times))
            #out <- ode(y = state, times = times, func = deterministic_ode_solve, parms = parameters)
            out <- data.frame(out)
            
            names(out)[-1] <- 1:parameters$dimension
            
            #Record the time when the opposite node of the initial is infected past a certain threshold
            past_threshold <- out[out[ , opp + 1] > 100, c(1, opp + 1)]   #general out[out$"opposite" > param$n / some number, c(1, opposite + 1)]
            record[j] <- past_threshold[1,1]
        }
        
        aggregate[i+1] <- mean(record, na.rm = TRUE)
    }
    #deterministic sim. stop when threshold reached in opposite side.
    params <- parameters[-3] #Just for bookkeeping 
    params <- c(params, "neighbors" = nei, "aggregate" = aggregate_runs)
    df <- data.frame("prob" = points, "time" = aggregate, "params" = params)
    df
}

find_cpath <- function(splits, aggregate_runs) { 
    aggregate <- rep(0, splits + 1)
    points <- gen_log_points(0.0001,1,splits, 10)
    points <- c(0, points)
    
    for(i in 0:splits) {
        record <- rep(0, aggregate_runs)
        
        for(j in 1:aggregate_runs) {
            g <- sample_smallworld(dim = 1, size = 1000, nei = 10, p = 1-sqrt(1-points[i+1]))
            #record[j] <- mean_distance(g, directed = FALSE)
            record[j] <- transitivity(g, type = "global", isolates = "NaN")
        }
        
        aggregate[i+1] <- mean(record, na.rm = TRUE)
    }
    #deterministic sim. stop when threshold reached in opposite side.
    df <- data.frame("prob" = points, "length" = aggregate)
    df
}

# rewire <- function(g, p) {
#     a <- get.adjacency(g, sparse = FALSE)
#     for(j in 1:dim(a)[1]) {
#         for(i in 1:dim(a)[1]) {
#             if(a[i,j] == 1) {
#                 total <- sum(a[i,])
#                 runif(1)
#             }
#         }
#         
# }

rewrite_adj <- function(adj = a, v) {
    #Rewrites the adjacency matrix to a reasonable one to put through the simulation
    diag(adj) <- 1
    columns <- colSums(adj)
    for(i in 1:dim(adj)[1]) {
        adj[i,i] <- 1 - (columns[i] - 1)*v
        adj[which(adj[,i] == 1), i] <- v
    }
    adj
}

gen_log_points <- function(start, end, freq, base) {
    #Give the starting and end points, and the number of points between, inclusive, to generate
    a = log(start, base)
    b = log(end, base)
    out = numeric(length = freq)
    for(i in 1:freq) {
        exp = a + (i-1)*(b-a)/(freq-1)
        out[i] = base^exp
    }
    out
}

plot_prob <- function(df, title = "Small world network") {
    base <- df$time[1]
    df <- df[-1,]
    p <- ggplot() + 
        geom_line(data = df, aes(x = prob, y = time / base), size = 1.5) +
        scale_x_log10(limits = c(0.0001, 1)) + xlab("Rewiring Probability") + ylab("Time(p)") +
        ylim(0, 1) + ggtitle(title) +
        theme(axis.title.x=element_text(vjust=-2, size = 15)) +
        theme(axis.title.y=element_text(angle=90, vjust=-1.5, size = 15)) +
        theme(plot.title=element_text(size=15, vjust=3)) +
        theme(plot.margin = unit(c(1,1,1,1), "cm"))
    
    p
}

# plot_network <- function(cpath, clustering) {
#     if(!is.na(cpath)) {
#         p <- ggplot()
#         length_base <- cpath$length[1]
#         cpath <- cpath[-1,]
#         p <- p + geom_line(data = cpath, aes(x = prob, y = length / length_base), size = 1.5)
#     }
#     if(!is.na(clustering)) {
#         if(!
# 
# }

get_info <- function(my_list) {
    output <- ""
    for(i in 1:length(my_list)) {
        a = names(my_list[i])
        b = my_list[[i]]
        output <- paste(output, a, "=", b, ",", sep = " ")
    }
    output
}

plot_all <- function(df1,df2,df3) {
    options(scipen=10000) #forces r not to abbreviate w/ sci notation
    base1 <- df1$time[1]
    df1 <- df1[-1,]
    df1 <- cbind(df1, "Params" = "v = 0.01, r = 0.001")
    base2 <- df2$time[1]
    df2 <- df2[-1,]
    df2 <- cbind(df2, "Params" = "v = 0.01, r = 0.01")
    base3 <- df3$time[1]
    df3 <- df3[-1,]
    df3 <- cbind(df3, "Params" = "v = 0.01, r = 0.1")

    p <- ggplot() + 
        geom_line(data = df1, aes(x = prob, y = time, color = Params), size = 1.5) +
        geom_line(data = df2, aes(x = prob, y = time, color = Params), size = 1.5) +
        geom_line(data = df3, aes(x = prob, y = time, color = Params), size = 1.5) +
        scale_x_log10(limits = c(0.0001, 1), breaks = c(0.0001,0.001,0.01,0.1,1), labels = c(0.0001,0.001,0.01,0.1,1)) + xlab("\nRewiring Probability") + ylab("T(p)\n")  +
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18, margin = margin(t = 20, l = 20, r = 20)),
              legend.text = element_text(size = 16), legend.title = element_text(size = 18, margin = margin(t = 10)), legend.key = element_blank(),
              plot.title = element_text(size = 18,margin = margin(b = 30))) + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
    p

}

plot_paths <- function(df1,df2) {
    options(scipen=10000) #forces r not to abbreviate w/ sci notation
    base1 <- df1$length[1]
    df1 <- df1[-1,]
    df1 <- cbind(df1, "Variable" = "L(Q) / L(0)")
    base2 <- df2$length[1]
    df2 <- df2[-1,]
    df2 <- cbind(df2, "Variable" = "C(Q) / C(0)")
    
    p <- ggplot() + 
        geom_point(data = df1, aes(x = prob, y = length/base1, shape = Variable), size = 3.0) +
        geom_point(data = df2, aes(x = prob, y = length/base2, shape = Variable), size = 3.0) +
        scale_x_log10(limits = c(0.0001, 1), breaks = c(0.0001,0.001,0.01,0.1,1), labels = c(0.0001,0.001,0.01,0.1,1)) + xlab("\nRewiring Probability") + ylab("Normalized Value\n")  +
        theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16, margin = margin(t = 20, l = 20, r = 20)),
              legend.text = element_text(size = 16), legend.title = element_text(size = 16, margin = margin(t = 10)), legend.key = element_blank(),
              plot.title = element_text(size = 18,margin = margin(b = 30))) + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
    p
}

check <- function() {
    environment()
    exists("p", where = environment())
}
