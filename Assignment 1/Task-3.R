# a.
N = 1000
mu = 0
sigma = 1

df = data.frame(
  "mu_vec" = rep(0, N),
  "variance_vec" = rep(0, N),
)

e <- list(mode="vector",length=N)

for (i in 1:N){
  e[[i]] <- rchisq(100, df = 2)
  df$mu_vec[i]=mean(e[[i]])
  df$variance_vec[i]=var(e[[i]])
}

hist(e[[1]], col = "blue")

# b. Our sample was choosen from the
norm <-rnorm(N, mean = mu, sd =sigma)

hist(df$mu_vec, col = "red")
hist(df$variance_vec, col = "orange")
hist(norm, col = "grey")

# plot( a, col=rgb(0,0,1,1/4), xlim=c(0,10))
# plot( b, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)
# plot( c, col=rgb(1,0,0,2/3), xlim=c(0,10), add=T)

# c. Mean follows t distribution, so the CTL can be applied

# d. CTL confirms for the mean
# 10000
N = 10000
df = data.frame(
  "mu_vec" = rep(0, N),
  "variance_vec" = rep(0, N),
)
e <- list(mode="vector",length=N)

for (i in 1:N){
  e[[i]] <- rchisq(100, df = 2)
  df$mu_vec[i]=mean(e[[i]])
  df$variance_vec[i]=var(e[[i]])
}

hist(e[[1]], col = "blue")
norm <-rnorm(N, mean = mu, sd =sigma)

hist(df$mu_vec, col = "red")
hist(df$variance_vec, col = "orange")
hist(norm, col = "grey")

# 100000
N = 100000
df = data.frame(
  "mu_vec" = rep(0, N),
  "variance_vec" = rep(0, N),
)
e <- list(mode="vector",length=N)

for (i in 1:N){
  e[[i]] <- rchisq(100, df = 2)
  df$mu_vec[i]=mean(e[[i]])
  df$variance_vec[i]=var(e[[i]])
}

hist(e[[1]], col = "blue")
norm <-rnorm(N, mean = mu, sd =sigma)

hist(df$mu_vec, col = "red")
hist(df$variance_vec, col = "orange")
hist(norm, col = "grey")

# 100000
N = 100000
df = data.frame(
  "mu_vec" = rep(0, N),
  "variance_vec" = rep(0, N),
)
e <- list(mode="vector",length=N)

for (i in 1:N){
  e[[i]] <- rchisq(100, df = 2)
  df$mu_vec[i]=mean(e[[i]])
  df$variance_vec[i]=var(e[[i]])
}

hist(e[[1]], col = "blue")
norm <-rnorm(N, mean = mu, sd =sigma)

hist(df$mu_vec, col = "red")
hist(df$variance_vec, col = "orange")
hist(norm, col = "grey")
