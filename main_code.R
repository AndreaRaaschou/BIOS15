#EXERCISE: Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
#Start by writing a function that computes the CV for a variable (see the Appendix for a brief introduction
#to writing functions in R). Then, simulate a random variable and write a loop that samples many times from
#this variable and computes the CV.

set.seed(1)

# Simulate a random variable x
x <- rnorm(100, 0, 1)

# Load my function compute_cv into this environment
source("compute_CV.R")

# Write loop that samples many times from x and computes the CV
out = NULL
for (i in 1:1000){
  smpl = sample(x, replace = TRUE)
  out[i]= compute_cv(smpl)
}

# Compute a 95 % confidence interval for the CV of x
lwr_q <- mean(x) - 1.96*mean(out)
uppr_q <- mean(x) + 1.96*mean(out)

# Print results
print(lwr_q)
print(uppr_q)
hist(out, las=1, main="")

