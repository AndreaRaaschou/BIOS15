# packeges for GLM:
library(MASS)
library(MuMIn)

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




# interesting to look at: is native vegetation better for regrowth of eucalyptus?








