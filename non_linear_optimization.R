library(optimx)
ans <- optimx(fn = function(x) sum(x*x),par = 1:2)
ans
coef(ans)

#Lets give an example of the two efficiency Curves
x <- seq(0,10000,step = 10)
a1 = 200
b1 = 0.13
a2 = 200
b2 = 0.14
plot(x,a1*x^b1)
points(x,a2*x^b2,col = "red")

igmb_fun <- function(a,b,x){a * x^b}

igmb_fun(a1,b1,2000)

objective_function <- function(){b1 * igmb_fun(a1,b1,x)}


dat = data.frame(x = c(12,43,34,12,78), y  = c(11,45,22,71,49))

min.RSS <- function(data, par) {
  with(data, sum((par[1] + par[2] * x - y)^2))
}

a=optim(par = c(0, 1), min.RSS, data = dat,control = list(trace =1))

plot(dat$x,dat$y)
points(10:80,a$par[2]*10:80+a$par[1],type = 'l')


params = data.frame(breakdown = c("MF","Mob","Ret"),
           alpha = c(233,23456,178),
           beta = c(0.23,0.9,0.5))

igmb <- function(reg_params,quarter_spend,opt_breakdown){
  sum(params$alpha * quarter_spend/pars ^params$beta)
  
}

library(nloptr)



igmb  = sum(params$alpha * c(9000,2332,324) ^params$beta)


eval_f <- function(x,params){
  return(list("objective" = sum(params$alpha * x ^params$beta),
              "gradient" = sum(params$alpha * params$beta *x^(params$beta -1 )))
  )
}

x = c(100,100,100)









