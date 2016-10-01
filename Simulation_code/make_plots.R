#Tests
require(ggplot2)
require(reshape2)

make_plots <- function(determ_df = out, stoch_df = stochastic, map_df = out_map) {
    names(map_df)[-1] <- 1:parameters$dimension
    map_df <- melt(map_df, id.vars = "time", variable.name = "patch", 
                      value.name = "population")
    map_df <- cbind(map_df, "model" = "map")
    
    determ_df <- data.frame(determ_df)
    names(determ_df)[-1] <- 1:parameters$dimension
    determ_df <- melt(determ_df, id.vars = "time", variable.name = "patch", 
                      value.name = "population")
    determ_df <- cbind(determ_df, "model" = "determ")
    
    names(stoch_df)[-1] <- 1:parameters$dimension
    stoch_df <- melt(stoch_df, id.vars = "Time", variable.name = "patch", 
                      value.name = "population")
    stoch_df <- cbind(stoch_df, "model" = "stoch")

    p <- ggplot() +
        geom_line(data = determ_df, aes(x = time, y = population, color = patch, linetype = model), size = 1.0) +
        #geom_line(data = stoch_df, aes(x = Time, y = population, color = patch, linetype = model),size = 1.0) +
        #geom_point(data = subset(map_df, time %% 40 == 1), aes(x = time, y = population, color = patch, linetype = model), size = 3.0) +
        geom_point(data = map_df, aes(x = time, y = population, color = patch, linetype = model), size = 3.0) +
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18, margin = margin(t = 20, l = 20, r = 20)),
              legend.text = element_text(size = 16), legend.title = element_text(size = 18, margin = margin(t = 10)), legend.key = element_blank(),
              plot.title = element_text(size = 18,margin = margin(b = 30))) + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()) + 
        xlab("\nTime") + ylab("Population\n") 
        #+ scale_x_discrete(labels = 0:10)
    
}

p <- make_plots()
p

#p <- make_plots()
#p

#To plot every Nth point, choose subset of data by 'data=subset(map_df, time %% N == 1)'