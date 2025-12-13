# Manipulate the data to prepare for analysis
library(tidyverse)

rm(list = ls())
dat = read.csv("datasets/exam2023_data-2.csv")

# Make new column with the sum of all eucalyptus seedlings
dat$euc_sdlgs = rowSums(dat[, c("euc_sdlgs0_50cm", "euc_sdlgs50cm.2m", "euc_sdlgs.2m")])

# Make new columns to sum different plant groups
dat$grass_cover = rowSums(dat[, c("ExoticAnnualGrass_cover", "ExoticPerennialGrass_cover", "NativePerennialGrass_cover")])
dat$fern_cover = dat[,"NativePerennialFern_cover"]
dat$shrub_cover = rowSums(dat[, c("ExoticShrub_cover", "NativeShrub_cover")])
dat$herb_cover = rowSums(dat[, c("ExoticAnnualHerb_cover", "ExoticPerennialHerb_cover", "NativePerennialHerb_cover")])
dat$graminoid_cover = dat[, "NativePerennialGraminoid_cover"]

# keep only relevant columns in dat
dat = dat[, c("SurveyID", "Season", "Property", "Landscape.position", 
              "grass_cover", "fern_cover", "shrub_cover",
              "herb_cover", "graminoid_cover", "euc_sdlgs")]

# Remove rows that have NA values (goes from 351 to 348 rows)
dat <- na.omit(dat) 
