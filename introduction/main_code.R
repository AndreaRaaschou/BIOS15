# First exercise of the course BIOS15

#Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
#Start by writing a function that computes the CV for a variable (see the Appendix for a brief introduction
#to writing functions in R). Then, simulate a random variable and write a loop that samples many times from
#this variable and computes the CV.

set.seed(1)

# Simulate a random variable x
x <- rnorm(100, 50, 1)

# Load my function compute_cv into this environment
source("compute_CV.R")

# Write loop that samples many times from x and computes the CV
out = NULL
for (i in 1:1000){
  smpl = sample(x, replace = TRUE)
  out[i]= compute_cv(smpl)
}

# Compute a 95 % confidence interval for the CV of x
conf <- quantile(out, c(0.025, 0.975))

# Print results
print(conf)
hist(out, las=1, main="Bootstrap CV")

# --------- The proportional properties of the natural log -----------------
#Use simulated data to show the close relationship between the SD of log-transformed data and the CV on
#arithmetic scale. You may need e.g. the rnorm function and a for-loop to achieve this. One strategy would
#be to start with comparing the two values for a single case, then build a matrix to hold the paired values,
#and finally use a for-loop to populate the matrix. 

# Simulate a random variable x
set.seed(1)
x <- rnorm(100, 50, 1)

# Initiate empty vairables to hold the bootstrapping results for cv and sd
out_SD = NULL
out_CV = NULL

# Bootstrapping and filling the empty variables
for (i in 1:1000){
  sample_x = sample(x, replace = TRUE)
  sample_log = log(sample_x)
  out_SD[i]= sd(sample_log)
  out_CV[i] = compute_cv(sample_x)
}

# Create dataframe with cv and sd as columns
df2 <- data.frame(cv = out_CV, sd = out_SD)

# Plot sd against cv
plot(df2$cv, df2$sd)
