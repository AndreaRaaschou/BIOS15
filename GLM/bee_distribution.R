# Bee distribution
library(MASS)
library(MuMIn)

rm(list = ls())

# Load data
dat = read.csv("datasets/Eulaema.csv")
head(dat)

# Try fitting a simple GLM assuming poisson distribution
m = glm(Eulaema_nigrita~effort+altitude+MAT, "poisson", data=dat) # What does the + mean in this model?
summary(m) 

# The residual deviance is almost 100 times larger than the residual degrees of freedom
# Therefore I will choose another model
m$deviance / m$df.residual

# Try fitting a model using the negative binomial dispersion instead
m = glm.nb(Eulaema_nigrita~effort+altitude+MAT, , data=dat)
summary(m) 

# Here, the residual deviance is about 1.2 times larger than the residual degrees of freedom 
# Indicates a good fit 
m$deviance / m$df.residual

# Look at the data using plots
par(mfrow=c(2,3))
plot(dat$effort, dat$Eulaema_nigrita, las=1)
plot(dat$altitude, dat$Eulaema_nigrita, las=1)
plot(dat$MAT, dat$Eulaema_nigrita, las=1)
hist(dat$effort, las=1)
hist(dat$altitude,  las=1)
hist(dat$MAT, las=1)

# The data seems to follow a poisson distribution with a low lambda-value - this is what you look at to choose model
par(mfrow=c(1,1))
hist(dat$Eulaema_nigrita, las=1)

# Get numbers for table and text analysis
# Slope and intercept:
# Slope will in this model be different for every fixed parameter and the same intercept for all of them
exp(m$coefficients)

# R-squared values
# Tells me how good the model is at explaining the variance of the data
r.squaredGLMM(m) # Get paper where it is explained what value to choose - this depends on what type of distribution the data has
1-(m$deviance/m$null.deviance) # Do not use this - old way of calculating it

# Try to make some good plot/plots of the prediction
# Lost here - what dependent variables to focus on? can only show one at a time
# Could also mimic the plot from the lecture with another parameter as lines (seed size)


# Example from lecture notes on plot 
par(mfrow=c(1,1))
plot(dat$MAT, dat$Eulaema_nigrita, las=1, col="darkgrey", pch=16)

# Prep to use the predict function
# 1) Make a smaller data frame where i set the values that i want for the 'other' fixed parameters
# one could have three different levels (for example mean, mean-sd, mean+sd)
# the other one should have only one value - the mean of that group
# put this smaller data frame as newdata

# x-axis values
xx <- seq(min(dat$MAT), max(dat$MAT), 0.01)

# Plot: Mean values for both altitude and effort ------------------------ marginal
effort_mean = mean(dat$effort, na.rm = TRUE)
alt_mean = mean(dat$altitude, na.rm = TRUE)
alt_min = min(dat$altitud)
alt_max = max(dat$altitud)

newdata_mean <- data.frame(
  MAT = xx,
  effort = effort_mean,
  altitude = alt_mean
)

y_hat = predict(m, newdata=newdata, type="response", se.fit = TRUE)
lines(xx, y_hat$fit)
polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)

# New plot: using different altitudes for the lines (min, mean and max) ------------------

newdata_min <- data.frame(
  MAT = xx,
  effort = effort_mean,
  altitude = alt_min
)
newdata_max <- data.frame(
  MAT = xx,
  effort = effort_mean,
  altitude = alt_max
)

y_hat_mean = predict(m, newdata=newdata, type="response", se.fit=T)
y_hat_min = predict(m, newdata=newdata_min, type="response", se.fit=T)
y_hat_max = predict(m, newdata=newdata_max, type="response", se.fit=T)

plot(dat$MAT, dat$Eulaema_nigrita, 
     las=1, col="darkgrey", pch=16,
     xlab = 'Mean Annual Temperature', 
     ylab = expression(paste(italic("Eulaema nigrita")," abundance")))

lines(xx, y_hat_mean$fit, col = 'red')
lines(xx, y_hat_min$fit, col = 'blue')
lines(xx, y_hat_max$fit, col = 'green')

legend('topleft', 
       col = c('green',
               'red', 
               'blue'), bty = 'n', lty = 1,
       legend = c('Altitude = Maximum', 
                  'Altitude = Mean', 
                  'Altitude = Minimum'))







