# This script is an appendix to the second report in the course
library(MASS)
library(MuMIn)

rm(list = ls())

# Load data
dat = read.csv("datasets/Eulaema.csv")
head(dat)

# Mean-center the MAP and MAT
dat$MATmc <- dat$MAT - mean(dat$MAT, na.rm = TRUE)
dat$MAPmc <- dat$MAP - mean(dat$MAP, na.rm = TRUE)

# Fit a Negative Binomial Generalized Linear Model to the data
m = glm.nb(Eulaema_nigrita ~ forest. + MATmc + MAPmc, data=dat)
summary(m) 
exp(m$coefficients)
r.squaredGLMM(m) 

# Visualize the data and look at potential correlation between the fixed parameters
par(mfrow=c(2,3))
plot(dat$forest., dat$MAPmc)
plot(dat$forest., dat$MATmc)
plot(dat$MAPmc, dat$MATmc)
plot(dat$forest., dat$Eulaema_nigrita)
plot(dat$MAPmc, dat$Eulaema_nigrita)
plot(dat$MATmc, dat$Eulaema_nigrita)

hist(dat$MATmc)
hist(dat$MAPmc)

par(mfrow = c(2,2))
plot(m)

# Plot prediction for E. nigrita abundance against forest cover with MAPmc and MATmc as different lines
x <- seq(min(dat$forest.), max(dat$forest.), 0.01)

newdata_mean <- data.frame(
  forest. = x,
  MAPmc = mean(dat$MAPmc, na.rm = TRUE),
  MATmc = mean(dat$MATmc, na.rm = TRUE)
)
newdata_lowMAPmc <- data.frame(
  forest. = x,
  MAPmc = mean(dat$MAPmc, na.rm = TRUE)- sd(dat$MAPmc, na.rm = TRUE),
  MATmc = mean(dat$MATmc, na.rm = TRUE) 
)
newdata_highMAPmc <- data.frame(
  forest. = x,
  MAPmc = mean(dat$MAPmc, na.rm = TRUE)+ sd(dat$MAPmc, na.rm = TRUE),
  MATmc = mean(dat$MATmc, na.rm = TRUE) 
)
newdata_lowMATmc <- data.frame(
  forest. = x,
  MAPmc = mean(dat$MAPmc, na.rm = TRUE),
  MATmc = mean(dat$MATmc, na.rm = TRUE) - sd(dat$MATmc, na.rm = TRUE)
)
newdata_highMATmc <- data.frame(
  forest. = x,
  MAPmc = mean(dat$MAPmc, na.rm = TRUE),
  MATmc = mean(dat$MATmc, na.rm = TRUE) + sd(dat$MATmc, na.rm = TRUE)
)

y_hat_mean = predict(m, newdata=newdata_mean, type="response", se.fit=T)
y_hat_lowMAPmc = predict(m, newdata=newdata_lowMAPmc, type="response", se.fit=T)
y_hat_highMAPmc = predict(m, newdata=newdata_highMAPmc, type="response", se.fit=T)
y_hat_lowMATmc = predict(m, newdata=newdata_lowMATmc, type="response", se.fit=T)
y_hat_highMATmc = predict(m, newdata=newdata_highMATmc, type="response", se.fit=T)

par(mfrow=c(1,2))
plot(dat$forest., dat$Eulaema_nigrita, 
     las=1, col="darkgrey", pch=16,
     xlab = 'Forest cover', 
     ylab = expression(paste(italic("Eulaema nigrita")," abundance")))
lines(x, y_hat_lowMAPmc$fit, col = 'red', lwd = '3')
lines(x, y_hat_mean$fit, col = 'orange', lwd = '3')
lines(x, y_hat_highMAPmc$fit, col = 'yellow', lwd = '3')
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
lines(x, y_hat_lowMATmc$fit, col = 'red', lwd = '3')
lines(x, y_hat_mean$fit, col = 'orange', lwd = '3')
lines(x, y_hat_highMATmc$fit, col = 'yellow', lwd = '3')
legend('topleft', 
       col = c('red',
               'orange', 
               'yellow'), bty = 'n', lty = 1,lwd = '3',
       legend = c('Mean annual temperature = Mean - SD', 
                  'Mean annual temperature = Mean', 
                  'Mean annual temperature = Mean + SD'))




