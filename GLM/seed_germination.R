# Seed germination
rm(list = ls())
setwd('/Users/andrearaaschou/courses/BIOS15/github/GLM')
dat = read.csv("datasets/dormancy.csv")
names(dat)

# Fit the model to one population
# Select seeds from the CC population
subdat = dat[dat$pop=="CC",] 

germ = subdat$germ2 * subdat$nseed #Successes = proportion of germinated seeds * nmbr of seeds
notgerm = subdat$nseed - germ #Failures

# Two ways of fitting a model to this data
mod1 = glm(cbind(germ, notgerm) ~ timetosowing, "binomial", data=subdat)
mod2 = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
logLik(mod1) == logLik(mod2)

# Calculate the duration of after-ripening required for the expected germination
# rate to be 0.5
summary(mod1)
coeffs <- mod1$coefficients
coeffs
x_0.5 <- - coeffs[1] / coeffs[2]
