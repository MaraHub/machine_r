weights_calc <- function(vec , level_of_prediction, skew){
  new_vec = level_of_prediction - vec 
  weights <- dnorm(new_vec,0,skew)
  return(weights)
}

smape <- function(actual,forecasted){
  smape <- 1 / (length(actual)) *sum( abs(forecasted - actual) / ((abs(actual) + abs(forecasted)) / 2)  )
  return(smape)
  
}


set.seed(1485)
len <- 20
level_of_prediction <- 0.2
x <- runif(len)
y <- x^3 + rnorm(len,0,0.1)
ds <- data.frame(x = x, y = y)
plot(y ~ x, main = "Known Cubic with noise",col = "red")
s <- seq(0,1,length = 100)
lines(s,s^3,lty = 2, col = "green")
fit1 <- nls(y ~ I(x^power), 
            data = ds , 
            start = list(power = 1 ) , 
            trace = T,
            weights = weights_calc(ds$x,level_of_prediction,0.4)
            )
m=summary(fit1)$coefficients[1]
lines(s,s^m,col = "blue")

smape(y,s^m)
smape(y,s^3)
m
#sum(weights)
plot (x,weights_calc(ds$x,level_of_prediction,1))

plot(x,hx)

