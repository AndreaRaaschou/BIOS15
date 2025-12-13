# the results are poisson-distributed count data. means i need to use a Generalized linear model (GLM)
par(mfrow = c(1,3))
hist(dat$euc_sdlgs0_50cm)
hist(dat$euc_sdlgs50cm.2m)
hist(dat$euc_sdlgs.2m)

# although not count-data, these variables are poisson distributed and have high 0-counts
par(mfrow = c(2,3))
hist(dat$ExoticAnnualGrass_cover)
hist(dat$ExoticAnnualHerb_cover)
hist(dat$ExoticPerennialGrass_cover)
hist(dat$ExoticPerennialHerb_cover)
hist(dat$NativePerennialGrass_cover)
hist(dat$NativePerennialHerb_cover)

# total vegetation cover should reasonably be bare ground cover - litter cover
par(mfrow = c(1,2))
hist(dat$BareGround_cover)
hist(dat$Litter_cover)