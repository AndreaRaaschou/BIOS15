# Bee distribution
rm(list = ls())

dat = read.csv("datasets/Eulaema.csv")
head(dat)

m = glm(Eulaema_nigrita~effort+altitude+MAT, "poisson", data=dat)
summary(m)

# To do:
# Answer questions in the text
# Interpret results
# Make some plots




