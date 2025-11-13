# My function to compute CV 
compute_cv <- function(x){
  # The cv value will be returned automatically
  sd(x) / mean(x)
}