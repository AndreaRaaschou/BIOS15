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

# Compute the means of the developmental time for each combination of larval and maternal host plant
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

# Make plot to illustrate the data
ggplot(dat, aes(x = LarvalHost,
                y = DevelopmentTime,
                color = MaternalHost,
                group = MaternalHost)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) + 
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Larval Host Plant",
       y = "Development Time",
       color = "Maternal Host") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 10, face = "italic"),
        axis.text.y = element_text(size = 10),
        legend.text  = element_text(size = 10, face = "italic") ) +
  scale_color_discrete(labels = c("BarbareaM" = "Barbarea",
                                  "BerteroaM" = "Berteroa"))+
  scale_x_discrete(labels = c("BarbareaL" = "Barbarea",
                              "BerteroaL" = "Berteroa"))

# Make a histogram of the dependent variable to see if the data follows a normal distribution
hist(dat$AdultWeight) 

# Linear regression and two-way ANOVA
m <- lm(dat$AdultWeight ~ dat$LarvalHost * dat$MaternalHost)
anova(m)
summary(m) 