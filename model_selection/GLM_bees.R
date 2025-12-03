# Model selection on Eulema data, generalized mixed models
library(MASS)
library(MuMIn)

rm(list = ls())

# Load data
dat = read.csv("datasets/Eulaema.csv")

# Optional fixed variables: (a bit unsure about if i am using this correctly?)
# MAT, MAP, altitude, forest.

# Mean-center the MAP and MAT
dat$MATmc <- dat$MAT - mean(dat$MAT, na.rm = TRUE)
dat$MAPmc <- dat$MAP - mean(dat$MAP, na.rm = TRUE)

# Fit Negative Binomial Generalized Linear Models to the data
m1 = glm.nb(Eulaema_nigrita ~ forest. * MATmc * MAPmc, data=dat)
m2 = glm.nb(Eulaema_nigrita ~ forest. + MATmc * MAPmc, data=dat)
m3 = glm.nb(Eulaema_nigrita ~ forest. + MATmc + MAPmc, data=dat)
m4 = glm.nb(Eulaema_nigrita ~ forest. * MAPmc, data=dat)
m5 = glm.nb(Eulaema_nigrita ~ forest. + MAPmc, data=dat)
m6 = glm.nb(Eulaema_nigrita ~ forest., data=dat)
m7 = glm.nb(Eulaema_nigrita ~ 1, data=dat)

# Put all fitted models into a list so we can loop over them easily
mlist = list(m1, m2, m3, m4, m5, m6, m7)

# Compute AIC values for all models and store them in a table
AICtab = AIC(m1, m2, m3, m4, m5, m6, m7)

# Add the log-likelihood for each model to the table
AICtab$logLik = unlist(lapply(mlist, logLik))

# Sort the table by AIC values (lowest AIC = best model)
AICtab = AICtab[order(AICtab$AIC, decreasing = FALSE), ]

# Compute delta AIC = difference from the best model's AIC
# The best model has delta = 0
AICtab$delta = round(AICtab$AIC - min(AICtab$AIC), 2)

# Convert delta AIC to relative likelihoods
# Models with higher delta get much lower likelihood
lh = exp(-0.5 * AICtab$delta)

# Compute Akaike weights (model probabilities)
# Each weight is: likelihood / total likelihood
# Weights sum to 1 and tell us the probability each model is the best
AICtab$w = round(lh / sum(lh), 2)

# Print the final ranked AIC table
AICtab

# ------------------------------------------------------------------------------------------------------------------------
# Biological interpretation:
# Best model m5 followed closely by m3

# 33 % likelihood that m5 is the correct model for the data

# logLik: A measure of how well the model fits the data without penalty for complexity 
# (higher value = better fit), m1 gives the highest logLik score (reasonable since it has the most parameters)

# AIC = model quality measure balancing fit vs. complexity
# Score below 2 is pretty much as good as the best fit (so m5, m3, m4 and m1 are competitors)











