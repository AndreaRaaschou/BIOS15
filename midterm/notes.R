# packeges for GLM:
library(MASS)
library(MuMIn)
library(tidyverse)
library(glmmTMB)

rm(list = ls())

dat = read.csv("datasets/exam2023_data-2.csv")
head(dat)

# investigate the data:
# is it continuous? count data? grouped?
# is it normally distributed?

# models to test:
# total vegetation cover
# the different types of vegetation
# testing to keep one type of vegetation static while varying the others
# testing to keep total ground cover static while varying the others
# have the time of testing as a random effect
# use total precipitation as a random effect for temperature or could possibly do a PCA for all climate variables to clump them together
# also think of rock cover? as a random effect

# columns to use:
# euc_sdl... - all sizes - add them together to one single row to make analyses easier (remember to argument for this in the report later)
# could also look at perennials (grasses, herbaceous, ferns, Graminoids), annuals (grasses and herbaceous) and shrubs
# i have: grasses, ferns, Graminoids, shrubs

#m = lm(dat$euc_sdlgs0_50cm ~ dat$ExoticAnnualGrass_cover + dat$NativePerennialGrass_cover)
#summary(m) - not good: data is not normally distributed

# Fit a  Generalized Linear Model using poisson distributed-errors and a log-link funciton to the data
#m = glm(euc_sdlgs50cm.2m ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + ExoticPerennialGrass_cover, data=dat, family = "poisson")
#summary(m) - not good: does not account for overdispersion


# Fit a negative binomial Generalized Linear Model to the data
m1 = glm.nb(euc_sdlgs50cm.2m ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + ExoticPerennialGrass_cover, data=dat)
summary(m2) # Use this type of model
r.squaredGLMM(m1)

# trying the same model on modified df
m = glm.nb(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + graminoid_cover, data=dat)
summary(m) 
r.squaredGLMM(m)


# interesting to look at: is native vegetation better for regrowth of eucalyptus?

# Now - try adding random effects to explain more of the variance
# This is covered in the mixed-effects chapter
# Several measurements are taken from the same land-owner (for example)
m2 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
               graminoid_cover + annual_precipitation + (1|Property), 
             data = dat, family = nbinom2)
summary(m2)
# random effects only for categorical variables
# Season variable collapses to zero (not enough groups) - therefore i removed this


# AIC
# I will remove variables in order of P-value (highest p-value will be removed)
m1 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
               graminoid_cover + annual_precipitation + (1|Property), 
             data = dat, family = nbinom2)
m2 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
               graminoid_cover + (1|Property), 
             data = dat, family = nbinom2)
m3 = glmmTMB(euc_sdlgs ~ grass_cover +  shrub_cover + herb_cover + 
               graminoid_cover + (1|Property), 
             data = dat, family = nbinom2)
m4 = glmmTMB(euc_sdlgs ~ grass_cover + shrub_cover + herb_cover + 
               (1|Property), 
             data = dat, family = nbinom2)
m5 = glmmTMB(euc_sdlgs ~ grass_cover + herb_cover + (1|Property), 
             data = dat, family = nbinom2)
m6 = glmmTMB(euc_sdlgs ~  herb_cover + (1|Property), 
             data = dat, family = nbinom2)
m7 = glmmTMB(euc_sdlgs ~ 1, 
             data = dat, family = nbinom2)

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









# Make a plot that looks like the GLM hand-in plots with seedlings on y-axis
# and the different types of vegetation on the x-axis

