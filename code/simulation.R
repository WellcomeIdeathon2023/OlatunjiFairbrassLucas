set.seed(123)

# Model 1
# ----------


# "True" causal model

N <- 100
temperature <- runif(N)
rice_yield <- temperature + rnorm(N, sd = 0.01)
suicide <-  -rice_yield + rnorm(N, sd = 0.01)

# right answer
# mts <- lm(suicide ~ temperature)
mrt <- lm(rice_yield ~ temperature)
msr <- lm(suicide ~ rice_yield)

m1 <- msr$coefficients['rice_yield'] * mrt$coefficients['temperature']
m1


# Wrong answer
# mts <- lm(suicide ~ temperature)
mtr <- lm(rice_yield ~ temperature)
mrs <- lm(suicide ~ rice_yield + temperature)

m11 <- mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']
m11


# # Right answer?
# # mts <- lm(suicide ~ temperature)
# mtr <- lm(rice_yield ~ temperature)
# mrs <- lm(suicide ~ rice_yield)
# # mrs2 <- lm(suicide ~ rice_yield + temperature)
#
# # indirect
# mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']
#
# # direct
# # mrs2$coefficients['temperature']




#  Model 2

# "True" causal model

N <- 100
temperature <- runif(N)
rice_yield <- temperature + rnorm(N, sd = 0.01)
suicide <-  -rice_yield + temperature + rnorm(N, sd = 0.01)


# Right answer?
# mts <- lm(suicide ~ temperature)   ## total effect
mtr <- lm(rice_yield ~ temperature)  ##
# mrs <- lm(suicide ~ rice_yield) # Nonsense
mrs2 <- lm(suicide ~ rice_yield + temperature)
mrs2
# indirect
m2 <- mrs2$coefficients['rice_yield'] * mtr$coefficients['temperature']
m2
# direct
mrs2$coefficients['temperature']
# confint(mrs2)





# Model 3
# ----------


# "True" causal model

N <- 100
temperature <- runif(N)
unobserved <- runif(N)
rice_yield <- temperature + 0.7 * unobserved + rnorm(N, sd = 0.01)
suicide <-  -rice_yield + 0.5*unobserved + 0*temperature +  rnorm(N, sd = 0.01)


# Right answer?
mtr <- lm(rice_yield ~ temperature + unobserved)
mrs <- lm(suicide ~ rice_yield + unobserved)
#mrs2 <- lm(suicide ~ rice_yield + temperature)

# indirect
m3 <- mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']
m3


# wrong  answer?
mtr <- lm(rice_yield ~ temperature + unobserved)
mrs <- lm(suicide ~ rice_yield + temperature + unobserved)

# indirect
m33 <- mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']
m33


# direct
# m33 <- mrs2$coefficients['temperature']
# confint(mrs2)

# both m3 and m33 are wrong when we pretend that we don't know the unobserved variables


# Model 4

# "True" causal model

N <- 100
temperature <- runif(N)
unobserved <- runif(N)
rice_yield <- temperature + 0.7 * unobserved + rnorm(N, sd = 0.01)
suicide <-  -rice_yield + 0.5*unobserved + 0.2*temperature +  rnorm(N, sd = 0.01)

# Right answer?
mtr <- lm(rice_yield ~ temperature + unobserved)
mtr
mts <- lm(suicide ~ rice_yield + temperature + unobserved)
mts

# indirect effect
m4 <- mtr$coefficients['temperature'] * mts$coefficients['rice_yield']
m4

# direct effect
m44 <- mts$coefficients['temperature']
m44




# Model 5

# "True" causal model

N <- 100
temperature <- runif(N)
unobserved <- runif(N)
rice_yield <- temperature + 0.7 * unobserved + rnorm(N, sd = 0.01)
suicide <-  -rice_yield + 0.5*unobserved + 0.2*temperature +  rnorm(N, sd = 0.01)

# Wrong answer?
mtr <- lm(rice_yield ~ temperature)
mtr
mts <- lm(suicide ~ rice_yield + temperature)
mts

# indirect effect
m5 <- mtr$coefficients['temperature'] * mts$coefficients['rice_yield']
m5

# direct effect
m55 <- mts$coefficients['temperature']
m55






##### Write a function to check at what value of the direct effect of temp on suicide to i completely get it wrong?

N <- 100
results <- matrix(NA, ncol = 3, nrow = N)
true_direct_effect <- seq(0.1, 1, length.out = N)
true_indirect_effect <-c()
for (i in 1:N) {
  temperature <- runif(N)
  unobserved <- runif(N)
  rice_yield <- temperature + 0.7 * unobserved + rnorm(N, sd = 0.01)
  suicide <- -rice_yield + 0.5 * unobserved + true_direct_effect[i] * temperature + rnorm(N, sd = 0.01)

  true_indirect_effect[i] <-  1 * -1


  mtr <- lm(rice_yield ~ temperature)
  mts <- lm(suicide ~ rice_yield + temperature)

  # Estimated indirect effect
  estimated_indirect_effect <- mtr$coefficients['temperature'] * mts$coefficients['rice_yield']

  # Store results
  results[i, 1] <- true_direct_effect[i]
  results[i, 2] <- estimated_indirect_effect
  results[i, 3] <- true_indirect_effect[i]
}

# Plot results
plot(results[, 1], results[, 2], xlab = "True Direct Effect", ylab = "Estimated Indirect Effect",
     main = "Comparison of Estimated Indirect Effect with True Direct Effect",
     pch = 16, col = "blue")

plot(results[, 1], results[, 3], xlab = "True Direct Effect", ylab = "True Indirect Effect",
     pch = 16, col = "red")
legend("topleft", legend = c("Estimated Indirect Effect", "True Indirect Effect"),
       pch = 16, col = c("blue", "red"))
