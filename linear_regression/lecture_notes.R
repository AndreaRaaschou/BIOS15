# Lecture notes from 13/11 -25 ------------------------------
set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)

plot(x, y, las = 1, # las = 1 makes the tick labels on the y-axis horizontal
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# Create linear model of y (dependent variable) dependent on x (independent variable)
# Save the fitted model object in m
m = lm(y~x)

cf <- m$coefficients
cf

# Compute predicted values of the linear model using coefficients
predvals = cf[1] + cf[2]*x

# Saving the old par settings before changing them
def_par <- par()

par(mfrow=c(1,2))
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
abline(m)
segments(x, y, x, predvals) # Draw lines from observed values to predicted values
hist(residuals(m), xlab="", las=1)
par(def_par) # Reset par settings

# Get info on the fitted model (residuals)
par(mfrow=c(2,2))
plot(m)
par(def_par) 

newx = seq(min(x), max(x), length.out=200)
predy = cf[1] + cf[2]*newx
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)

summary(m)
