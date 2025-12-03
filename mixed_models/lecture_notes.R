# Lecture notes on generalized linear mixed models (GLMM)
library(glmmTMB)

rm(list = ls())

set.seed(145)
x1 = rnorm(200, 10, 2)

# Add random group-level effects (10 groups, 20 observations each)
groupmeans = rep(rnorm(10, 20, 4), each=20)

# Add a factor identifying the group for each observation
groupID = as.factor(rep(paste0("Group", 1:10), each = 20))

# Create response y intercept using slope effect of x1, group effect, and random noise 
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

# Plot y vs x1, color points by group
plot(x1, y, col = as.numeric(groupID), las = 1)

# Add y, x1, and groupID into a data frame
data = data.frame(y, x1, groupID)
head(data)

# Add a random intercept for groupID in the model
m = glmmTMB(y ~ 1 + (1|groupID), data = data)

# Display the model estimates and random effect variance
summary(m)

# Extract only the part that is related to the random effect
VarCorr(m)

# Extract the variance among groups and within groups from the model
VarAmongGroups = attr(VarCorr(m)$cond$groupID, "stddev")^2
VarWithinGroups = attr(VarCorr(m)$cond, "sc")^2

VarAmongGroups
VarWithinGroups

# Calculate the actual variance of the simulated group means for comparison
var(groupmeans)

# Calculate the percent of the variance explained by groups, by dividing by the total estimated variance
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100

# Calculate the square coefficient of variation (CV^2)
CV2_Among = VarAmongGroups / mean(y)^2
CV2_Within = VarWithinGroups / mean(y)^2
CV2_Total = CV2_Among + CV2_Within

# Put all values in a table
df = data.frame(
  Mean = mean(x1),          # mean of predictor x1
  SD = sd(x1),              # standard deviation of x1
  Among = VarAmongGroups / (VarAmongGroups + VarWithinGroups) * 100,  # % variance among groups
  Within = VarWithinGroups / (VarAmongGroups + VarWithinGroups) * 100, # % variance within groups
  CV2_Among,
  CV2_Within,
  CV2_Total
)
# Round variables for clarity
df = apply(df, MARGIN=2, FUN=round, digits=2)



