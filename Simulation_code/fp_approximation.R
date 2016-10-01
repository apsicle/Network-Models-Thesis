# Newton's method for 2d system

rootsolve <- function(x_0, y_0) {
    x_n = x_0
    y_n = y_0
    while()
    J <- matrix(c(((((1+x_n*(exp(r)-1))*(a*exp(r))) - (a*x_n*exp(r)*(exp(r)-1))) / (1+ x_n*(exp(r)-1))^2),
                ((((1+x_n*(exp(r)-1))*(c*exp(r))) - (c*x_n*exp(r)*(exp(r)-1))) / (1+ x_n*(exp(r)-1))^2),
                ((((1+y_n*(exp(r)-1))*(b*exp(r))) - (b*y_n*exp(r)*(exp(r)-1))) / (1+ y_n*(exp(r)-1))^2),
                ((((1+y_n*(exp(r)-1))*(d*exp(r))) - (d*y_n*exp(r)*(exp(r)-1))) / (1+ y_n*(exp(r)-1))^2)),2,2)
                
    f <- matrix(c())
}
