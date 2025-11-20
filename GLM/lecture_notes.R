# Lecture notes on GLM
library(MuMIn)
rm(list = ls())

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x)) # What is the purpose of this inversion?

# Using the logit link function to tranform data to a normal distribution -----------------------------
x = runif(200)
logit_x = logit(x)

par(mfrow=c(2,2))
hist(x, las=1)
hist(logit_x, las=1)

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1, # why is this curve produced by doing this?
     xlab="Logit (x)",
     ylab="P")
plot(x, invlogit(logit_x), las=1) # whats the purpose of this graph?


# Alternative link function: probit -----------------------------------------------------------------
# Can use inbuilt functions pnorm and qnorm to get the link function and its inverse
par(mfrow=c(1,1))
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit/Probit (x)",
     ylab="P")
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"),
       lty=c(1,2), bty="n")


# Logistic regression --------------------------------------------------------------------
# The steps below will create binomial distributed data "backwards"
# Generate 200 normally distributed, random values, mean 10 and sd 3
x = rnorm(200, 10, 3)

# Formulate a linear predictor (η=β0+β1x+noise)
eta = -2 + 0.4*x + rnorm(200, 0, 2)

# Transform predicted values into probabilities
p = invlogit(eta)

# Binarize the data by sampling from the binomial distribution
# Explanation of rbinom: generates vector with a number of successes
# of 200 binomial experiments including 1 trial where the probability 
# of success in each trial is given by p.
y = rbinom(200, 1, p)

par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)

# Now, fit the generalized linear model, specify error family and link function
m = glm(y~x, family=binomial(link="logit")) 
summary(m)

# EXERCISE: Replicate plot
# Get the coefficients from the summary
coefs = summary(m)$coef 
coefs

# Define x-values that spans the data range along the x-axis
x_pred = seq(from=min(x), to=max(x), by=0.01)

# Get predicted values y_hat using the model coefficients (intercept and slope)
y_hat = coefs[1,1] + coefs[2,1]*x_pred

# Transform the y_hat values to the probability scale to obtain predicted probabilities p_hat
p_hat = invlogit(y_hat)

# Find the log odds of 0 (corresponding to a probability of 0.5) 
# Solve the model equation (to get y_hat) for 0 to find the predictor value corrsponding 
# to a probability of 0.5 (relevan bilogical benchmark)
x_b <- -coefs[1,1] / coefs[2,1]

# PLOT
# Main plot with data binomial distribution - am I using correct variables?
par(mfrow=c(1,1))
plot(x, y, las=1,
     xlab="x",
     ylab="y")

# Add curve 
lines(x_pred, p_hat, lty=1)

# Add straight lines
abline(h=0.5, lty=2) # correct
abline(v=x_b, lty=2) 

# Estimating variance or how well the model fits the data ------------------------------
# Estimating the variance through the delta method
r.squaredGLMM(m)

# Tjur's D
y_hat = coefs[1,1] + coefs[2,1]*x
p_hat = invlogit(y_hat)
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)])
# QUESTION: how can the last function know which y's are 1 and 0?

# Final notes ----------------------------------------------------------------------
# If the data containts 1s and 0s, use the model 
glm(y ~ x, family=binomial(link="logit"))

# If each observation is based on more than one trial, the model can be formulated as:
glm(y ~ x, family=binomial(link="logit"), weights=n)
# or 
glm(cbind(successes, failures) ~ x, family=binomial(link="logit"))


