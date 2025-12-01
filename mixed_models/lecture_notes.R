library(glmmTMB)
rm(list = ls())

set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)
plot(x1, y, col=as.numeric(groupID), las=1)

data = data.frame(y, x1, groupID)
head(data)
m = glmmTMB(y ~ 1 + (1|groupID), data=data)
summary(m)
