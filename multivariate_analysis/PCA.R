# Data exercise: Principal Component Analysis
# Perform a principal component analysis and produce a biplot
# Interpret the results
# Treat the data before analysis (e.g. z-transforming or log-transforming) to 
# avoid more variable variables to dominate the resulting principal components
library(MASS)
library(ellipse)
rm(list = ls())

dat = read.csv("datasets/blossoms.csv")
head(dat)

# Questions:
# How to treat the data?

# First, look at the variability of each variable
# Focus on ASD, GAD and GSD (different distance measurements of the flower parts)
par(mfrow = c(1,3))
hist(dat$ASD) # Slightly left-orianted - do a log transformation? Also higher sd
hist(dat$GAD)
hist(dat$GSD)

# Get means and standard deviations for the traits of interest
data_traits <- matrix(c(mean(dat$ASD, na.rm = TRUE), mean(dat$GAD, na.rm = TRUE), mean(dat$GSD, na.rm = TRUE), 
                        sd(dat$ASD, na.rm = TRUE), sd(dat$GAD, na.rm = TRUE), sd(dat$GSD, na.rm = TRUE)),
                      nrow = 3, ncol = 2,
                      byrow = FALSE, 
                      dimnames = list(c('ASD', 'GAD', 'GSD'), 
                                      c('mean', 'sd')))


mean(log(dat$ASD), na.rm = TRUE) # 1.19 - better but not much better
# in what situation would one use log transformation instead of z-transformation? could i combine them?

# Z-transform all three traits
# Use formula: (xi - x_mean)/sd(x), but for simplicity using the scale() function
dat$ASDz <- as.vector(scale(dat$ASD))
dat$GADz <- as.vector(scale(dat$GAD))
dat$GSDz <- as.vector(scale(dat$GSD))

# Traits are now mean-centered and re-scaled to have a sd of 1
hist(dat$ASDz) 
hist(dat$GADz)
hist(dat$GSDz)

# make dataframe with only the columnw i want to use
X = data.frame(ASDz = dat$ASDz,
               GADz = dat$GADz,
               GSDz = dat$GSDz)
x = data.frame(ASD = dat$ASD,
               GAD = dat$GAD,
               GSD = dat$GSD)

# Remove rows with NA values
X <- na.omit(X)
x <- na.omit(x)

# Perform the PCA - what is going wrong here?
pca_z = princomp(X)
summary(pca_z)


# Repeat without z-transformation
pca = princomp(x)
summary(pca)

par(mfrow = c(1,2))
biplot(pca, col=c("grey", "black"), cex=c(.5, 1))
biplot(pca_z, col=c("grey", "black"), cex=c(.5, 1))

par(mfrow = c(1,1))
biplot(pca, col=c("grey", "black"), cex=c(.5, 1)) 
biplot(pca_z, col=c("grey", "black"), cex=c(.5, 1))

# plot in different way - could also add the eigenvectors to the plot
means = c(apply(x[,1:2], 2, mean))
plot(x$ASD, x$GAD, las=1, col="grey")
lines(ellipse(cov(x[,1:2]), centre=means))

# Biological interpretation: size is the most important PC,
# but shape is also important (half of the importance, 2.1 vs 0.97 covariance diagonal)
# is this correct interpretation? - yes. size contributes almost three times more than shape to the variance in the data
eigen(cov(x))
eigen(cov(X))

# Other interpretation: ASD and GAD not correlated, GAD AND GSD closely correlated - also correct
