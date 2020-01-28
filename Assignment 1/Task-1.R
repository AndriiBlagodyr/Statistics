# a. We can make the conclusion, that the Volatility decreases with the sample size and becomes close to 1 
mu = 1
sigma = 1
"1c"
zc = qnorm(1-0.05/2)
n = seq(from=100, to=100000, by=100)

N = length(n)
df = data.frame(
  "mu_vec" = rep(0, N),
  "sigma_vec" = rep(0, N),
  "conf_int_lower" = rep(0, N),
  "conf_int_upper" = rep(0, N)
)
for (i in 1:length(n)){
  measurements = rnorm(n[i], mean = mu, sd =sigma)
  df$mu_vec[i]=mean(measurements)
  df$sigma_vec[i]=sd(measurements)
  
# b.
  df$conf_int_lower[i] = df$mu_vec[i] -zc*df$sigma_vec[i]/sqrt(n[i])
  df$conf_int_upper[i] = df$mu_vec[i] +zc*df$sigma_vec[i]/sqrt(n[i])
}

plot(n, df$mu_vec, type = "l")
lines(n, df$conf_int_lower, col = "blue")
lines(n, df$conf_int_upper, col = "red")
# d
plot(n, df$sigma_vec, type = "l")


# b. Confidence interval formula X???1.96??x ??? µ ??? X +1.96??x. So in our case 1.96*1/sqrt(n) = 0.01
# From the current expression n is equal to  n  =  38416"

# d. The sample variance caracterises the range of values near the mean. Since the sample size is large enough 
# (n???30), the sample variance S ^ 2 is a fairly good estimator of ??^2."
