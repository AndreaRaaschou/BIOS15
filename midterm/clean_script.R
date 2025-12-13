library(MASS)
library(MuMIn)
library(tidyverse)
library(glmmTMB)

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

# Remove outlier with 88 euc_sdlgs
mean(dat$euc_sdlgs)

(88 - 28) / sd(dat$euc_sdlgs)
dat <- dat[dat$euc_sdlgs < 80, ]



# Testing models with different random effects to find which ones explain substantial variance
m_re1 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
               graminoid_cover + (1|Property) + (1|Landscape.position) + (1|Season), 
             data = dat, family = nbinom2)
m_re2 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
                  graminoid_cover + (1|Property) + (1|Landscape.position), 
                data = dat, family = nbinom2)
m_re3 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
                  graminoid_cover + (1|Property), 
                data = dat, family = nbinom2)
m_re4 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
                  graminoid_cover, 
                data = dat, family = nbinom2)

# AIC analysis (random effects)
mlist_re = list(m_re1, m_re2, m_re3, m_re4) 
AICtab_re = AIC(m_re1, m_re2, m_re3, m_re4) # Compute AIC values 
AICtab_re = AICtab_re[order(AICtab_re$AIC, decreasing = FALSE), ] # Sort the table by AIC values 
AICtab_re$logLik = unlist(lapply(mlist_re, logLik)) # Compute the log-likelihood 
AICtab_re$delta = round(AICtab_re$AIC - min(AICtab_re$AIC), 2) # Compute delta AIC  
lh = exp(-0.5 * AICtab_re$delta)
AICtab_re$w = round(lh / sum(lh), 2) # Compute Akaike weights (model probabilities)
AICtab_re # Print the final ranked AIC table
# Continue with only Property as random effect


# Fit models with decreasing number of fixed effects
# Using a generalized linear mixed model using a negative binomial distribution and a log link function
m1 = glmmTMB(euc_sdlgs ~ grass_cover + fern_cover + shrub_cover + herb_cover + 
               graminoid_cover + (1|Property), 
             data = dat, family = nbinom2)
m2 = glmmTMB(euc_sdlgs ~ grass_cover +  shrub_cover + herb_cover + 
               graminoid_cover + (1|Property), 
             data = dat, family = nbinom2)
m3 = glmmTMB(euc_sdlgs ~ grass_cover + shrub_cover + herb_cover + (1|Property), 
             data = dat, family = nbinom2)
m4 = glmmTMB(euc_sdlgs ~ grass_cover + herb_cover + (1|Property), 
             data = dat, family = nbinom2)
m5 = glmmTMB(euc_sdlgs ~  herb_cover + (1|Property), 
             data = dat, family = nbinom2)
m6 = glmmTMB(euc_sdlgs ~ 1, 
             data = dat, family = nbinom2)

# AIC analysis (fixed effects)
mlist = list(m1, m2, m3, m4, m5, m6) 
AICtab = AIC(m1, m2, m3, m4, m5, m6) # Compute AIC values 
AICtab = AICtab[order(AICtab$AIC, decreasing = FALSE), ] # Sort the table by AIC values 
AICtab$logLik = unlist(lapply(mlist, logLik)) # Compute the log-likelihood 
AICtab$delta = round(AICtab$AIC - min(AICtab$AIC), 2) # Compute delta AIC  
lh = exp(-0.5 * AICtab$delta)
AICtab$w = round(lh / sum(lh), 2) # Compute Akaike weights (model probabilities)
AICtab # Print the final ranked AIC table

summary(m1)
summary(m5)

# Plot m5
x <- seq(0, 100,length.out = 200)
newdata <- data.frame(
  herb_cover = x,
  Property = NA  # population-level prediction
)
y_hat <- predict(m5, newdata = newdata, type = "response", se.fit = TRUE)

# Extract random effect sd 
re_sd <- sqrt(as.numeric(VarCorr(m5)$cond$Property)) 

# Upper and lower bounds for shaded area 
# the sd will be on the log scale, therefore take the exp() and multiply with the mean values
y_upper <- y_hat$fit * exp(re_sd)  
y_lower <- y_hat$fit * exp(-re_sd)

# Plot 
par(mfrow = c(1,1))
plot(dat$herb_cover, dat$euc_sdlgs,
     las = 1, col = "darkgrey", pch = 16,
     xlab = "Herb cover (%)",
     ylab = "Eucalyptus seedlings")

# Shaded polygon for random effect variance
polygon(c(x, rev(x)),
        c(y_upper, rev(y_lower)),
        col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)

# Population-level line
lines(x, y_hat$fit, col = "orange", lwd = 2)

# Add legend
legend("topright",
       legend = c("Population-level prediction", "Random effect variance"),
       col = c("orange", rgb(0.5, 0.5, 0.5, 0.3)),
       lwd = c(3, NA),
       pch = c(NA, 15),           
       pt.cex = 2,                
       bty = "n")        

# Scatter plots of each vegetation type vs eucalyptus seedlings
veg_cols <- c("grass_cover", "fern_cover", "shrub_cover", "herb_cover", "graminoid_cover")
veg_data <- dat[, veg_cols]
labels <- c("Grass cover (%)", "Fern cover (%)", "Shrub cover (%)", "Herb cover (%)", "Graminoid cover (%)")

par(mfrow = c(1,5), 
    mar = c(4.5, 3.9, 2, 1), # margins
    cex = 0.8) # text setting
for (i in seq_along(veg_cols)) {
  plot(dat[[veg_cols[i]]], dat$euc_sdlgs,
       xlab = labels[i],
       ylab = ifelse(i == 1, "Eucalyptus seedlings", NA),  
       pch = 19,
       las = 2,
       col = rgb(0,0,1,0.3),
       xlim = c(0, 100))
}

