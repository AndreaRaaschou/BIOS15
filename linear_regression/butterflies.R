# Analyzing a factorial experiment using ANOVA

library(ggplot2)

# Remove all variables 
rm(list = ls())

# Set working directory
setwd("/Users/andrearaaschou/courses/BIOS15/github/linear_regression/")

# Upload data 
dat = read.csv("datasets/butterflies.csv")

# Add markers to the hosts to indicate if they are maternal or larval hosts
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")

# Make a histogram of the dependent variable to see if the data follows a normal distribution
hist(dat$DevelopmentTime) # Not normally distributed - would it make sense to normalize by taking log?

# Compute the means of the developmental time for each combination of larval and maternal host plant
means <-  tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

# Compute the standard errors for the means using the standard deviation
sd <- aggregate(DevelopmentTime ~ MaternalHost + LarvalHost, 
             data = dat,
             FUN = function(x) sd(x)/sqrt(length(x)))
sd

# Make plot to illustrate the data
ggplot(dat, aes(x = LarvalHost,
                y = DevelopmentTime,
                color = MaternalHost,
                group = MaternalHost)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Larval host",
       y = "Developmental time (days)",
       color = "Maternal host") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 10, face = "italic"),
        axis.text.y = element_text(size = 10),
        legend.text  = element_text(size = 10, face = "italic"),
        legend.position = c(0.25, 0.8)) +
  scale_color_discrete(labels = c("BarbareaM" = "Barbarea",
                                  "BerteroaM" = "Berteroa"))+
  scale_x_discrete(labels = c("BarbareaL" = "Barbarea",
                              "BerteroaL" = "Berteroa"))

# Linear regression and two-way ANOVA
m <- lm(dat$DevelopmentTime ~ dat$LarvalHost * dat$MaternalHost)
anova(m)
summary(m) 

# Calculate the percentage of variance explained by the different independent variables -------------
# Using SS(variable) / SS(total)

# Larval host:
2809.15 / (2809.15 + 496.87 + 80.80 + 992.05)

# Maternal host:
496.87 / (2809.15 + 496.87 + 80.80 + 992.05)

# Larval host * Maternal host: 
80.80 / (2809.15 + 496.87 + 80.80 + 992.05)

# Residuals:
992.05 / (2809.15 + 496.87 + 80.80 + 992.05)

# Calculate the increase in developmental time for the different independent groups ----------------
# Eq: (new value - original value) / original value
# In this case: (Ber - Bar) / Bar

# Larval host:
lh <- tapply(dat$DevelopmentTime, dat$LarvalHost, mean)
(lh[2] - lh[1]) / lh[1] 

# Maternal host:
mh <- tapply(dat$DevelopmentTime, dat$MaternalHost, mean)
(mh[2] - mh[1]) / mh[1] 

# Larval host * Maternal host: * 2 - the increase is different - therefore there is an interaction
# Maternal host barbarea: BarBar - BarBer / BarBar
(means[1,2] - means[1,1]) / means[1,1]
# Maternal host Berteroa: BerBar - BerBer / BerBar
(means[2,2] - means[2,1]) / means[2,1]
