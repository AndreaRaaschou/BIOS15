# Lecture notes on principle component analysis
library(MASS)
library(ellipse)
rm(list = ls())

# Create a 3×3 covariance matrix C (this defines how strongly the three traits covary)
C = matrix(c(0.7, 0.2, -0.3,
             0.2, 1.2, 0.4,
             -0.3, 0.4, 0.6),
           nrow=3)

# s. 1: how to compute the covariance? cov ≠ cor. the exercise is about producing the correlation
# automatically using nested for-loops and calculating the correlation using the covariance and the variance
# that one can get from the covariance matrix (C in this case).

# Simulate 200 samples from a multivariate normal distribution with mean vector (0,0,0)
# and covariance matrix C (this generates traits z1, z2, z3 with the correlation structure defined by C)
set.seed(1)
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma=C))

# Assign column names to make the variables readable (z1, z2, z3)
colnames(X) = c("z1", "z2", "z3")
head(X)

# Compute the sample means of z1 and z2, which will be used as the center of the covariance ellipse
means = c(apply(X[,1:2], 2, mean))


# Plot z1 vs z2 to visualize their sample relationship
plot(X$z1, X$z2, las=1)
# Draw a covariance ellipse for z1 and z2, centered at their sample means
lines(ellipse(cov(X[,1:2]), centre=means))


# s. 3: leading eigenvector can be interpreted roughly as the size of the organism ...
# does this mean that lambda = 2 implies that the size of the organism is 2?
# Answer: no, it means that the leading eigenvalue correlates with the trait (not direct measurement)
# that has the most variance, which often - in a biological setting (ecology) is size


# Plot z1 vs z2 again, this time with eigenvectors drawn as arrows
plot(X$z1, X$z2, las=1, col="grey")
lines(ellipse(cov(X[,1:2]), centre=means))
# Draw the first eigenvector (direction of maximum variance)
arrows(means[1], means[2],
       means[1] + eigen(C)$vectors[1,1],
       means[2] + eigen(C)$vectors[2,1],
       code=2, length=0.1, lwd=2)
# Draw the second eigenvector (direction of second-largest variance)
arrows(means[1], means[2],
       means[1] + eigen(C)$vectors[1,2],
       means[2] + eigen(C)$vectors[2,2],
       code=2, length=0.1, lwd=2)


# Check the dimensions of X and the eigenvectors to make sure that matrix multiplication is ok
dim(as.matrix(X))
dim(as.matrix(eigen(C)$vectors[,1]))

# The following steps is a manual way to do a PCA analysis - should probably produce the same results

# Project the data onto the first eigenvector (this creates the first principal component t1)
t1 = as.matrix(X) %*% eigen(C)$vectors[,1]
# Project the data onto the second eigenvector (second principal component t2)
t2 = as.matrix(X) %*% eigen(C)$vectors[,2]
# Project the data onto the third eigenvector (third principal component t3)
t3 = as.matrix(X) %*% eigen(C)$vectors[,3]

# Compute the sample variances of the original traits z1, z2, z3 (original measurements)
c(var(X[,1]), var(X[,2]), var(X[,3]))
# Variance for the new traits t1, t2, t3. Will be in descending order
c(var(t1), var(t2), var(t3))

# The sum of the variances is still the same
var(t1) + var(t2) + var(t3)
var(X[,1]) + var(X[,2]) + var(X[,3])

# Doing PCA analysis using the R package prcomp
pca = princomp(X)
summary(pca)

# s. 6: Do not understand the text last on the page. "The proportion of variance explained by each ...
# s. 6: How do i interpret the summary(pca)-table?

# Try to make an covariance matrix based on X
C_correct = cov(X)
C_correct

# using the correct covariance matrix, the proportion of variances are the same
pca$sdev^2/sum(pca$sdev^2)
eigen(C)$values/sum(eigen(C)$values)
eigen(C_correct)$values/sum(eigen(C_correct)$values)

# To understand how each principal component is constructed, 
# look at the loadings of each original variable onto the PCs - do not understand this either?
# What is a principal component? - same as comp1, comp2 or PC1, PC2 ...
pca$loadings[1:3, 1:3] 

# Plot the principal components and the original measurements/traits as arrows
biplot(pca, col=c("grey", "black"), cex=c(.5, 1))





# Starting Principal component regression 

# Simulate data from multivariate normal distribution
XX = as.data.frame(scale(X))
y = 0.5*XX$z1 -0.3*XX$z2 + 0.2*XX$z3 + rnorm(200, 0, 1)

# Fit a standard linear regression model
m0 = lm(y ~ XX$z1 + XX$z2 + XX$z3)
summary(m0)

# Next, perform a principal component analysis, extract the three principal components, 
# and fit a second linear models with the PCs as predictors
pca = princomp(XX)
par(mfrow = c(1,1))
biplot(pca, col=c("grey", "black"), cex=c(.5, 1))

pc1 = pca$scores[,1]
pc2 = pca$scores[,2]
pc3 = pca$scores[,3]

m3 = lm(y ~ pc1 + pc2 + pc3)
summary(m3)

# s. 10: do not understand B = Qb and the text on the top of the page
Q = pca$loadings
b = as.matrix(summary(m3)$coefficients[-1,1])
Q %*% b

# Fitting a model to only the first principal component
m1 = lm(y~pc1)
summary(m1)

# Look at the loadings of each original variable onto the PCs
pca$loadings[1:3, 1:3]
