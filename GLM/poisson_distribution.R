# Poison and negative-binomial regression
library(MASS)

rm(list = ls())

# First, produce the data following a poisson distribution
x = rpois(200, 3)
hist(x, las=1)

x = seq(0, 20, 1)
y = dpois(x, lambda=1)
plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
legend("topright", col=1:3, pch=16,
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
                expression(paste(lambda, " = 10"))))

x = rnorm(200, 10, 3)
eta = -2 + 0.2*x

# Generates 200 poisson-distributed numbers with mean 0.3 (could be done in the previous step)
# take the exp - makes all values positive, then round up to nearest integer with ceiling
y = ceiling(exp(eta + rpois(200, 0.3)))

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

# Start analysis
m = glm(y~x, family="poisson")
summary(m)

par(mfrow=c(1,1))
plot(x, y, las=1, col="darkgrey", pch=16)

xx = seq(min(x), max(x), 0.01)

# Use predict-function to make the plot
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T)

# Plot lines of the prediction
lines(xx, y_hat$fit)
#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)
polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)

r.squaredGLMM(m) # What does the different values mean? How can they be interpreted?
1-(m$deviance/m$null.deviance) # not sure how to interpret this either?
# My understanding (ask if it is correct):
# Null deviance = constant, the deviance if a null model was fitted to the data
# Residual deviance = chancing, becomes smaller with a model that is better at explaining the data
# So, the pseudo r-squared value becomes larger as the model is better = the residual deviance decrease

# What is the log likelihood of a model? p. 13 - what would one use this for? to choose the best model?

# Simulate overdispersed data
set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu=.8))) 

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m = glm(y~x, family="poisson")
summary(m)

1-(m$deviance/m$null.deviance)

# using the negative binomial dispersion instead of the poisson dispersion in the model
m = glm.nb(y~x)
summary(m)
1-(m$deviance/m$null.deviance)

