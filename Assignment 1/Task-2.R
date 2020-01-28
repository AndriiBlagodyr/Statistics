n = 100
x = rt(n,df=5)

loglikelihood <- function(df, x) {
  sum(log10(((1 + (x**2)/df)**(-(df+1)/2)) / ((beta(df/2, 0.5) * (df)**(0.5)))))
}

result <- optim(5, loglikelihood, x=x, method = "BFGS" )
result


