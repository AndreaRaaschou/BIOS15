# Lecture notes on principle component regression
rm(list = ls())

dat = read.csv('datasets/alpineplants.csv')
dat = na.omit(dat)
head(dat)

# Start by doing a PCA on the temperature variables
dat_temp <- data.frame(mean_T_winter = dat$mean_T_winter,
                       max_T_winter = dat$max_T_winter, 
                       min_T_winter = dat$min_T_winter,
                       mean_T_summer = dat$mean_T_summer,
                       max_T_summer = dat$max_T_summer, 
                       min_T_summer = dat$min_T_summer)
dat_temp <- na.omit(dat_temp)

pca_temp = princomp(dat_temp)
summary(pca_temp)
# Conclusion: two first components stand for 94 % of the variation - use these

# Extract the scores, use pc1 and pc2 instead of all previous temp columns to do my new model
# Could also do just pc1 - use AIC to choose which model to use
pc11 = pca_temp$scores[,1]
pc12 = pca_temp$scores[,2]

# Do the same thing for altitude, soil moisture and snow
dat_alt <- data.frame(snow = dat$snow,
                      soil_moist = dat$soil_moist,
                      altitude = dat$altitude)

dat_alt <- na.omit(dat_alt)
pca_alt = princomp(dat_alt)
summary(pca_alt) # Use only the first component

pc21 = pca_alt$scores[,1]

m1 = lm(dat$Thalictrum.alpinum ~ pc11 + pc12 + pc21 + dat$light)
summary(m1)
m2 = lm(dat$Thalictrum.alpinum ~ pc11 + pc21 + dat$light)
summary(m2)

m3 = lm(dat$Thalictrum.alpinum ~ pc11 + pc21)
summary(m3)

m4 = lm(dat$Thalictrum.alpinum ~ pc11)
summary(m4)

# Now, how would i interpret this back to the original temperatures?
# Use the formula for loadings somehow





