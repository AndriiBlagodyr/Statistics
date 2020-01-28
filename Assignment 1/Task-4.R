# 4. Assess the probability of a type I error (alpha)

# a.

n = 100
mu_0 = 500
stdev = sqrt(50)
alpha = 0.04
my_sample = rnorm(n, mu_0, stdev)

# t test statistic

t_test = (mean(my_sample)-mu_0)/sd(my_sample)

# t critical value for alpha=0.04

qt(0.98, df=n-1) # Two sided test (0.02 on each side)

# Answer: We cannot reject H0 as the test statistic is not large enough

# b. Check results using R

t.test(my_sample, mu = mu_0, conf.level = 1-alpha/2)

# Answer: The p-value is higher than alpha so we cannot reject H0.
test = t.test(my_sample, mu = mu_0, conf.level = 1-alpha/2)

# c. 

M = 1000
n = 100
mu_0 = 500
stdev = sqrt(50)
alpha = 0.04

rejected = rep(0, M)

for (i in 1:M){
  
  my_sample = rnorm(n, mu_0, stdev)
  
  test = t.test(my_sample, mu = mu_0, conf.level = 1-alpha/2)
  rejected[i] = test$p.value < alpha
  
}

alpha_hat = sum(rejected)/M
alpha_hat

# As we are using samples coming from an actual normal distribution, the difference should be equal to 0 on average.
# A larger number of samples (M) would reduce the variability of that number. 
# A variation in alpha hat would mean the distribution is not normal and our assumptions not met.

# d.

M = 1000
n = 100
mu_0 = 500
stdev = sqrt(50)
alpha = 0.04

rejected = rep(0, M)

for (i in 1:M){
  
  zi = rt(n, df=3)
  my_sample = 500+zi/sqrt(abs(zi))*50
  
  test = t.test(my_sample, mu = mu_0, conf.level = 1-alpha/2)
  rejected[i] = test$p.value < alpha
  
}

alpha_hat = sum(rejected)/M
alpha_hat

# The rejection rate will be similar, as the sample size is large enough, the average will be close to a normal distribution (CLT).
# Also, thestandard deviation is identical than earlier and that both distribution are symmetrical.

# e.


run_test <- function(df=2){
  
  M = 1000
  n = 100
  mu_0 = 500
  stdev = sqrt(50)
  alpha = 0.04
  
  test_norm = function(q){
    pnorm(q,mean=mu_0, sd = stdev)
  }
  
  not_rejected = rep(0, M)
  
  for (i in 1:M){
    
    zi = rt(n, df=df)
    my_sample = 500+zi/sqrt(abs(zi))*sqrt(50)
    
    test = ks.test(my_sample, pnorm, mean = mu_0, sd = stdev)
    not_rejected[i] = test$p.value > alpha
  }
  
  beta_hat = sum(not_rejected)/M
  beta_hat
  
}

beta_df = rep(0,length(2:50))

for (i in 1:49){
  
  beta_df[i] = run_test(i+1)
}

plot(beta_df)

# We see the power increases up to a point, as df increases, so does the t distribution gets similar to a normal - hence the test does not reject it correctly more often.
# The evaluate the power it would have been more interesting to vary the sample size for fixed df...


# Check t calibration
zi = rt(n, df=3)
my_sample = 500+zi/sqrt(abs(zi))*50
sd(my_sample)
mean(my_sample)

