# Make a boxplot of the different vegetation types and the spread of their ground coverage
veg_data <- dat[, c("grass_cover", "fern_cover", "shrub_cover", "herb_cover", "graminoid_cover")]

labels <- c("Grass", "Fern", "Shrub", "Herb", "Graminoid")

boxplot(veg_data,
        main = "Vegetation Cover",
        ylab = "Percent cover",
        col = "grey",
        names = labels,
        las = 2)  

veg_means <- colMeans(veg_data, na.rm = TRUE) 

# Scatter plots of each vegetation type vs eucalyptus seedlings
par(mfrow = c(3,2))
for (veg in veg_cols) {
  plot(dat[[veg]], dat$euc_sdlgs,
       xlab = veg,
       ylab = "Eucalyptus seedlings",
       main = paste("Eucalyptus seedlings vs", veg),
       pch = 19,
       col = rgb(0,0,1,0.5))
}


# plot with grass_cover
x <- seq(min(dat$grass_cover, na.rm = TRUE), 
         max(dat$grass_cover, na.rm = TRUE), 
         length.out = 100)

# New data for predictions, keeping herb_cover at its mean
newdata <- data.frame(
  grass_cover = x,
  herb_cover = mean(dat$herb_cover, na.rm = TRUE),
  Property = NA   # population-level prediction
)

# Predict from the model
y_hat <- predict(m4, newdata = newdata, type = "response", se.fit = TRUE)

# Plot observed data
par(mfrow = c(1,1))
plot(dat$grass_cover, dat$euc_sdlgs,
     las = 1, col = "darkgrey", pch = 16,
     xlab = "Grass cover (%)",
     ylab = "Eucalyptus seedlings")

# Add predicted line
lines(x, y_hat$fit, col = "orange", lwd = 2)






x <- seq(min(dat$herb_cover), max(dat$herb_cover), length.out = 100)

newdata <- data.frame(
  herb_cover = x,
  grass_cover = mean(dat$grass_cover),
  Property = NA   # population-level prediction, random intercept ignored
)

y_hat <- predict(m4, newdata = newdata, type = "response", se.fit = TRUE)

par(mfrow = c(1,1))
plot(dat$herb_cover, dat$euc_sdlgs,
     las = 1, col = "darkgrey", pch = 16,
     xlab = "Herb cover (%)",
     ylab = "Eucalyptus seedlings")

# Add prediction lines
lines(x, y_hat$fit, col = "orange", lwd = 2)












x <- seq(min(dat$herb_cover), max(dat$herb_cover),length.out = 100)

# Population-level prediction 
newdata <- data.frame(
  herb_cover = x,
  grass_cover = mean(dat$grass_cover),
  Property = NA  # population-level prediction
)

y_hat <- predict(m4, newdata = newdata, type = "response", se.fit = TRUE)

# Extract random effect SD 
re_sd <- sqrt(as.numeric(VarCorr(m4)$cond$Property)) 

# Upper and lower bounds for shaded area 
# the sd will be on the log scale, therefore take the exp() and multiply with the mean values
y_upper <- y_hat$fit * exp(re_sd)  
y_lower <- y_hat$fit * exp(-re_sd)

# Plot 
par(mfrow = c(1,1))
plot(dat$herb_cover, dat$euc_sdlgs,
     las = 1, col = "darkgrey", pch = 16,
     xlab = "Herb cover (%)",
     ylab = "Eucalyptus seedlings",
     main = "Eucalyptus seedlings vs Herb cover")

# Shaded polygon for random effect variance
polygon(c(x, rev(x)),
        c(y_upper, rev(y_lower)),
        col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)

# Population-level line
lines(x, y_hat$fit, col = "orange", lwd = 3)



