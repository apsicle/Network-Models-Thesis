#Tests
require(ggplot2)
require(reshape2)

make_plots <- function(dimension) {
    if(dimension == 2) {
        p <- ggplot() +
            geom_line(data = deterministic_ode, aes(x = Time, y = N1), color = "blue4") + 
            geom_line(data = stochastic, aes(x = Time, y = N1), color = "red4") +
            geom_line(data = stochastic, aes(x = Time, y = N2), color = "red") +
            geom_line(data = deterministic_ode, aes(x = Time, y = N2), color = "blue")
    }
    else if(dimension == 3) {
        p <- ggplot() +
            #geom_line(data = deterministic_ode, aes(x = Time, y = N1), color = "blue4") + 
            #geom_line(data = stochastic, aes(x = Time, y = N1), color = "red4") +
            #geom_line(data = test_out, aes(x = Time, y = N1), color = "green4") +
            geom_line(data = stochastic, aes(x = Time, y = N2), color = "red") +
            geom_line(data = deterministic_ode, aes(x = Time, y = N2), color = "blue") +
            geom_line(data = test_out, aes(x = Time, y = N2), color = "green2") +
            geom_line(data = deterministic_ode, aes(x = Time, y = N3), color = "royalblue") +
            geom_line(data = stochastic, aes(x = Time, y = N3), color = "tomato") +
            geom_line(data = test_out, aes(x = Time, y = N3), color = "greenyellow")
    }    
}

p <- make_plots(3)
p
