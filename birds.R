# Fitting a linear regression to real data --------------------

# Download data using read.csv
setwd("/Users/andrearaaschou/courses/BIOS15/github/")
birds <- read.csv("datasets/bird_allometry.csv")

# Split data into females and males
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

# Fit linear models to log-transformed brain and body-sizes
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)
ms = lm(log(brain_mass)~log(body_mass)*Sex, data=birds)

# Check residuals (want them to be normally distributed)
hist(residuals(mm))

def_par <- par()
par(mfrow=c(2,2))
plot(mf)
par(def_par)

# Look at the model specifics
summary(mm)
summary(mf)
summary(ms)

# Make scatterplots of the results
plot(log(males$body_mass), log(males$brain_mass), las=1,
     xlab="Body mass",
     ylab="Brain mass")
abline(mm)

plot(log(females$body_mass), log(females$brain_mass), las=1,
     xlab="Body mass",
     ylab="Brain mass")
abline(mf)

# Which species do the outliers belong to?
for (i in 1:nrow(birds)){
  if (log(birds$body_mass[i]) > 9){
    print(birds$Genus_Species[i])
  }
}

