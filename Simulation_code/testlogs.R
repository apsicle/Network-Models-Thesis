# Log Tests

#1) Our Formulation
test1log <- function(S_n, r_0) {
    y <- rep(NA, 1000)
    fd <- rep(NA, 1000)
    y[1] <- S_n
    fd[1] <- 0
    for(i in 1:999) {
        y[i+1] <- (exp(r_0)*y[i]) / (1 + y[i]*((exp(r_0) - 1)/1))
        fd[i+1] <- y[i]
    }
    fd1 <<- fd
    y
}

plot_sample <- function(x, y) {
    options(scipen=10000) #forces r not to abbreviate w/ sci notation
    df1 <- data.frame(cbind(x,y))
    
    p <- ggplot() + 
        geom_line(data = df1, aes(x = x, y = y), size = 1.50) +
        #geom_point(data = df2, aes(x = prob, y = length/base2, shape = Variable), size = 3.0) +
        xlab("\nTime t") + ylab("N(t)\n")  +
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18, margin = margin(t = 20, l = 20, r = 20)),
              legend.text = element_text(size = 16), legend.title = element_text(size = 16, margin = margin(t = 10)), legend.key = element_blank(),
              plot.title = element_text(size = 18,margin = margin(b = 30))) + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
    p
}

#2) Standard Formulation of 1-d logistic map
test2log <- function(S_n, r_0) {
    y <- rep(NA, 200)
    fd <- rep(NA, 200)
    y[1] <- S_n
    fd[1] <- 0
    for(i in 1:199) {
        y[i+1] <- r_0*y[i]*(1-y[i]/1)
        fd[i+1] <- y[i+1] - y[i]
    }
    fd2 <<- fd
    y
}

#3) Ricker map
test3log <- function(S_n, r_0) {
    y <- rep(NA, 200)
    fd <- rep(NA, 200)
    y[1] <- S_n
    for(i in 1:199) {
        y[i+1] <- r_0*y[i]*exp(-y[i])
        fd[i+1] <- y[i+1] - y[i]
    }
    fd3 <<- fd
    y
}

#4) Integration of continuous logistic, nonautomous form
test4log <- function(S_n, r_0) {
    y <- rep(NA, 200)
    for(i in 1:200) {
        y[i] <- (1*S_n) / (S_n + (1 - S_n)*exp(-r_0*i))
    }
    y
}

refresh <- function(S_n, r_0) {
    y1 <<- test1log(S_n, r_0)
    y2 <<- test2log(S_n, r_0)
    y3 <<- test3log(S_n, r_0)
    y4 <<- test4log(S_n, r_0)
}