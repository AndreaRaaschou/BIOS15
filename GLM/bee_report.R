# This script is an appendix to the second report in the course
library(MASS)
library(MuMIn)

rm(list = ls())

# Load data
dat = read.csv("datasets/Eulaema.csv")
head(dat)

# Fit a Negative Binomial Generalized Linear Model to the data
m = glm.nb(Eulaema_nigrita~forest.+MAT+MAP, , data=dat)
summary(m) 
exp(m$coefficients)
r.squaredGLMM(m) 

# Visualize the data and look at potential correlation between the fixed parameters
par(mfrow=c(2,3))
plot(dat$forest., dat$MAP)
plot(dat$forest., dat$MAT)
plot(dat$MAP, dat$MAT)
plot(dat$forest., dat$Eulaema_nigrita)
plot(dat$MAP, dat$Eulaema_nigrita)
plot(dat$MAT, dat$Eulaema_nigrita)

hist(dat$MAT)
hist(dat$MAP)

par(mfrow = c(2,2))
plot(m)

# Plot prediction for E. nigrita abundance against forest cover with MAP and MAT as different lines
x <- seq(min(dat$forest.), max(dat$forest.), 0.01)

newdata_mean <- data.frame(
  forest. = x,
  MAP = mean(dat$MAP, na.rm = TRUE),
  MAT = mean(dat$MAT, na.rm = TRUE)
)
newdata_lowMAP <- data.frame(
  forest. = x,
  MAP = mean(dat$MAP, na.rm = TRUE)- sd(dat$MAP, na.rm = TRUE),
  MAT = mean(dat$MAT, na.rm = TRUE) 
)
newdata_highMAP <- data.frame(
  forest. = x,
  MAP = mean(dat$MAP, na.rm = TRUE)+ sd(dat$MAP, na.rm = TRUE),
  MAT = mean(dat$MAT, na.rm = TRUE) 
)
newdata_lowMAT <- data.frame(
  forest. = x,
  MAP = mean(dat$MAP, na.rm = TRUE),
  MAT = mean(dat$MAT, na.rm = TRUE) - sd(dat$MAT, na.rm = TRUE)
)
newdata_highMAT <- data.frame(
  forest. = x,
  MAP = mean(dat$MAP, na.rm = TRUE),
  MAT = mean(dat$MAT, na.rm = TRUE) + sd(dat$MAT, na.rm = TRUE)
)

y_hat_mean = predict(m, newdata=newdata_mean, type="response", se.fit=T)
y_hat_lowMAP = predict(m, newdata=newdata_lowMAP, type="response", se.fit=T)
y_hat_highMAP = predict(m, newdata=newdata_highMAP, type="response", se.fit=T)
y_hat_lowMAT = predict(m, newdata=newdata_lowMAT, type="response", se.fit=T)
y_hat_highMAT = predict(m, newdata=newdata_highMAT, type="response", se.fit=T)

par(mfrow=c(1,2))
plot(dat$forest., dat$Eulaema_nigrita, 
     las=1, col="darkgrey", pch=16,
     xlab = 'Forest cover', 
     ylab = expression(paste(italic("Eulaema nigrita")," abundance")))
lines(x, y_hat_lowMAP$fit, col = 'red', lwd = '3')
lines(x, y_hat_mean$fit, col = 'orange', lwd = '3')
lines(x, y_hat_highMAP$fit, col = 'yellow', lwd = '3')
legend('topleft', 
       col = c('red',
               'orange', 
               'yellow'), bty = 'n', lty = 1,lwd = '3',
       legend = c('Mean annual precipitation = Mean - SD', 
                  'Mean annual precipitation = Mean', 
                  'Mean annual precipitation = Mean + SD'))

plot(dat$forest., dat$Eulaema_nigrita, 
     las=1, col="darkgrey", pch=16,
     xlab = 'Forest cover', 
     ylab = expression(paste(italic("Eulaema nigrita")," abundance")))
lines(x, y_hat_lowMAT$fit, col = 'red', lwd = '3')
lines(x, y_hat_mean$fit, col = 'orange', lwd = '3')
lines(x, y_hat_highMAT$fit, col = 'yellow', lwd = '3')
legend('topleft', 
       col = c('red',
               'orange', 
               'yellow'), bty = 'n', lty = 1,lwd = '3',
       legend = c('Mean annual temperature = Mean - SD', 
                  'Mean annual temperature = Mean', 
                  'Mean annual temperature = Mean + SD'))

unique(dat$method)



