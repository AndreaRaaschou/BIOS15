# Hurdle model

y1 = ((y>1)*1)
m1 = glm(y1~x, family="binomial" (link="logit"))

y2 = y
y2[which(y==0)] = NA

m2 = glm(y2~x, family="poisson", na=na.exclude)

coefs1 = summary(m1)$coef
coefs2 = summary(m2)$coef

y_hat1 = coefs1[1,1] + coefs1[2,1]*x
y_hat2 = coefs2[1,1] + coefs2[2,1]*x
y_pred = invlogit(y_hat1)*exp(y_hat2)

par(mfrow=c(1,3))
plot(x, invlogit(y_hat1), las=1)
plot(x, exp(y_hat2), las=1)
plot(x, y_pred, las=1)