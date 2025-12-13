# Make a boxplot of the different vegetation types and the spread of their ground coverage
veg_cols <- c("grass_cover", "fern_cover", "shrub_cover", "herb_cover", "graminoid_cover")
veg_data <- dat[, veg_cols]
labels <- c("Grass cover (%)", "Fern cover (%)", "Shrub cover (%)", "Herb cover (%)", "Graminoid cover (%)")

par(mfrow = c(1,1))
boxplot(veg_data,
        main = "Vegetation Cover",
        ylab = "Percent cover",
        col = "grey",
        names = labels,
        las = 2)  

veg_means <- colMeans(veg_data, na.rm = TRUE) 
veg_means
# Scatter plots of each vegetation type vs eucalyptus seedlings
par(mfrow = c(1,5), 
    mar = c(4.5, 3.9, 2, 1), 
    oma = c(0, 2, 0, 0),
    cex = 0.8)
for (i in seq_along(veg_cols)) {
  plot(dat[[veg_cols[i]]], dat$euc_sdlgs,
       xlab = labels[i],
       ylab = ifelse(i == 1, "Eucalyptus seedlings", NA),  
       pch = 19,
       las = 2,
       col = rgb(0,0,1,0.3),
       xlim = c(0, 100))
  abline(v = veg_means[i], col = "red", lwd = 2)
}


