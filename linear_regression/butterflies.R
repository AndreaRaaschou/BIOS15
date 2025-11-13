# Analyzing a factorial experiment using ANOVA

# Set working directory
setwd("/Users/andrearaaschou/courses/BIOS15/github/")

# Upload data 
dat = read.csv("datasets/butterflies.csv")

# Add markers to the hosts to indicate if they are maternal or larval hosts
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

# Compute the means of the developmental time for each combination of larval and maternal host plant
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)

# Make plot to illustrate the model/data

# Could make either a boxplot with four different "boxes" or mimic the line-plot in fig 2 in the lecture notes
# Like maternal host - color and larval host - x-axes













