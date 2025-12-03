# Exercise on variance partitioning with random-effects models
# Using the butterflies data set and mother ID as my random grouping variable
library(glmmTMB)
rm(list = ls())

dat = read.csv("datasets/butterflies.csv")
