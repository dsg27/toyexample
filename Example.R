# A simple fictional example (Height to Calorie Consumption) to illustrate the benefits of Bayesian analysis ####
set.seed(42)
n = 100 #number of observations in fictional sample

# 1. Create a data generation process ####
height <- rnorm( n = n, mean = 175, sd = 15) #Assume UK population height distribution
individual_variation <- rnorm(n = n, mean = 0, sd = 100) #Assume some random variation in calorific needs e.g. activity, muscle mass, metabolism, ...
calories <- 12*height + individual_variation #Assume a simple data generation process (DGP)
data <- data.frame( calories, height) #data produced by DGP

# 2. Visualise data ####
par(mfrow = c(2,1))
hist(data$height) #visualise distribution of heights
hist(data$calories) #visualise distribution of calories

# 3. Inference process ####
library(rethinking) #devtools::install_github("rmcelreath/rethinking")

# 3.1 Define and Visualise Model ####
# Visualise priors
par(mfrow = c(1,3))
plot( seq(11,13,0.01), dnorm(seq(11,13,0.01), 12, 0.1), type = "l", main = "Expert prior for beta" )
plot( seq(-1000,1000,0.01), dnorm(seq(-1000,1000,0.01), 0, 100), type = "l", main = "Expert prior for constant")
plot( seq(0,2000,0.01), dunif(seq(0,2000,0.01), 0, 2000), type = "l", main = "Expert prior for noise")

# Define inference model
f <- alist(
  calories ~ dnorm(mu, sigma),
  mu <- a + b*height,
  a ~ dnorm(0, 100),
  b ~ dnorm(12, 0.1),
  sigma ~ dunif(0, 2000)
  )

# 3.2 Infer model parameters via 'Maximum A Posteriori' (Bayesian estimation of mode of distribution)
fit_map <- map(f, data = data)
precis(fit_map, prob = 0.95)

# 3.3 Infer model parameters via OLS (Frequentist estimation of mean of distribution)
ols <- lm(calories ~ height, data = data)
coef(ols)
#summary(ols)

# Infer model parameters via MLE (should make no difference as they are equivalent in this case)
mle <- glm(calories ~ height, data = data, family = gaussian)
coef(mle)

# 3.4 Infer model parameters via Posterior mean (Bayesian estimation of mean of distribution)
fit_stan <- map2stan( 
  f , 
  data = data, 
  start = list(a = 0, b = 0, sigma = 100),
  iter = 10000
)
pairs(fit_stan)
coef(fit_stan)

# 4. Compare model parameters
precis(fit_map)
precis(fit_stan)
summary(ols)
