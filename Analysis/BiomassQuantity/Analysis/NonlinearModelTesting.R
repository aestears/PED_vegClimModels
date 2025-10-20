#///////////////////
# Trying some model approaches to model biomass as a function of cover
# Alice Stears
# 07/22/2025
#///////////////////


# load packages -----------------------------------------------------------
library(tidyverse)
library(gnm)

# load data ---------------------------------------------------------------

dat <- readRDS("./Data_processed/BiomassQuantityData/GEDIbiomass_predictedCover_climateAndSoils.rds")

# make min. biomass value not 0
dat[dat$totalBiomass_MgHect == 0 & !is.na(dat$totalBiomass_MgHect), "totalBiomass_MgHect"] <- 
  dat[dat$totalBiomass_MgHect == 0 & !is.na(dat$totalBiomass_MgHect), "totalBiomass_MgHect"] + .01

# scale the predictors
name_lookup <- c(
  tmean_s = "tmean", 
  prcpTempCorr_s = "prcpTempCorr", 
  isothermality_s = "isothermality",
  annWatDef_s ="annWatDef",
  prcp_s = "prcp", 
  prcp_seasonality_s = "prcp_seasonality", 
  prcp_dry_s = "prcp_dry", 
  annWetDegDays_s = "annWetDegDays",
  t_warm_s = "t_warm", 
  t_cold_s = "t_cold",
  prcp_wet_s = "prcp_wet", 
  soilDepth_s = "soilDepth", 
  sand_s = "sand", 
  coarse_s = "coarse",
  AWHC_s = "AWHC", 
  clay_s = "clay", 
  carbon_s = "carbon")

dat_scaled <- dat %>% 
  select(tmean, prcpTempCorr, isothermality, annWatDef,
         prcp, prcp_seasonality, prcp_dry, annWetDegDays,
         t_warm, t_cold, prcp_wet, soilDepth, sand, coarse, AWHC, clay, carbon) %>% 
  dplyr::rename(all_of(name_lookup)) %>% 
  scale() 

dat <- dat %>% 
  cbind(dat_scaled) %>% 
  select(1:37,40:62)

# specify model with gnm --------------------------------------------------
#μ=(β 1⋅climate+β2⋅soils)⋅f(cover;β3,β4)
# Here, climate and soils have linear effects (beta1, beta2) while beta3, beta4 are estimated through Nonlin().
# get data for shrub cover
dat_shrub <- dat %>% 
  select(totalBiomass_MgHect, shrubCover, tmean_s, prcpTempCorr_s, isothermality_s, annWatDef_s,
         prcp_s, prcp_seasonality_s, prcp_dry_s, annWetDegDays_s,
         t_warm_s, t_cold_s, prcp_wet_s, soilDepth_s, sand_s, coarse_s,
         AWHC_s, clay_s, carbon_s, x, y) %>% 
  drop_na()

# make sure that shrub cover is > 0 
dat_shrub[dat_shrub$shrubCover==0, "shrubCover"]

## use glm to get starting values for the gnm
glm_fit <- glm(totalBiomass_MgHect ~ tmean_s + prcpTempCorr_s +  isothermality_s + annWatDef_s + prcp_s +
                 prcp_seasonality_s + prcp_dry_s + annWetDegDays_s + t_warm_s + t_cold_s + prcp_wet_s + soilDepth_s +
                 sand_s + coarse_s + AWHC_s + clay_s + carbon_s + offset(log((shrubCover + median(shrubCover))/shrubCover)),
               data = dat_shrub,
               family = Gamma(link = "log"))
# Then use coef(glm_fit) to initialize beta_j (for the linear terms) in your gnm start vector.

# Construct cover term manually (ensures numerical stability)
# Provide initial guesses for beta18 and beta19
beta18 <- 1
beta19 <- 1

dat_shrub$shrubCover_term <- log((beta18 + dat_shrub$shrubCover) / (beta19 * dat_shrub$shrubCover))

# Check for infinite or NA values
stopifnot(all(is.finite(dat_shrub$shrubCover_term)))

# Fit model with Gamma(log) family
fit_gnm <- gnm(
  totalBiomass_MgHect ~ 
    tmean_s + prcpTempCorr_s +isothermality_s+ annWatDef_s+
                          prcp_s+ prcp_seasonality_s+ prcp_dry_s+ annWetDegDays_s+
                          t_warm_s+ t_cold_s+ prcp_wet_s+ soilDepth_s+ sand_s+ coarse_s+
                          AWHC_s+ clay_s+ carbon_s+ shrubCover_term
  ,
  #start = c(coef(glm_fit)[2:18], shrubCover_term = 1),
  data = dat_shrub,
  family = Gamma(link = "log")
)

## now, use optimization to estimate beta18 and beta19
# Define optimization objective: negative log-likelihood
neg_loglik <- function(par, data) {
  beta18 <- par[1]
  beta19 <- par[2]
  
  # Constrain parameters to be positive
  if (beta18 <= 0 || beta19 <= 0) return(1e10)
  
  # Compute transformed cover term
  data$shrubCover_term <- log((beta18 + data$shrubCover) / (beta19 * data$shrubCover))
  
  # Try fitting the Gamma model
  fit <- try(gnm(
    totalBiomass_MgHect ~ 
      tmean_s + prcpTempCorr_s +isothermality_s+ annWatDef_s+
      prcp_s+ prcp_seasonality_s+ prcp_dry_s+ annWetDegDays_s+
      t_warm_s+ t_cold_s+ prcp_wet_s+ soilDepth_s+ sand_s+ coarse_s+
      AWHC_s+ clay_s+ carbon_s+ shrubCover_term,
    data = data,
    family = Gamma(link = "log"),
    trace = FALSE
  ), silent = TRUE)
  
  # If model fails, return large penalty
  if (inherits(fit, "try-error")) return(1e10)
  
  # Return negative log-likelihood
  -logLik(fit)
}

# Starting values
init_vals <- c(beta18 = 1, beta19 = 1)

# Run optimization
opt <- optim(
  par = init_vals,
  fn = neg_loglik,
  data = dat_shrub,
  method = "L-BFGS-B",
  lower = c(1e-3, 1e-3),   # prevent log(0) or div-by-zero
  upper = c(1000, 1000)
)

# Best estimates
best_beta18 <- opt$par[1]
best_beta19 <- opt$par[2]
cat("Estimated beta18:", best_beta18, "\n")
cat("Estimated beta19:", best_beta19, "\n")

## Now, refit the final model
# Add optimized nonlinear term to data
dat_shrub$shrubCover_term  <- log((best_beta18 + dat_shrub$shrubCover) / (best_beta19 * dat_shrub$shrubCover))

# Fit final model
final_fit <- gnm(
  totalBiomass_MgHect ~ 
    tmean_s + prcpTempCorr_s +isothermality_s+ annWatDef_s+
    prcp_s+ prcp_seasonality_s+ prcp_dry_s+ annWetDegDays_s+
    t_warm_s+ t_cold_s+ prcp_wet_s+ soilDepth_s+ sand_s+ coarse_s+
    AWHC_s+ clay_s+ carbon_s+ shrubCover_term,
  data = dat_shrub,
  family = Gamma(link = "log")
)

summary(final_fit)

# Predict using this model ------------------------------------------------
# 1. Generate a sequence of cover values across observed range
cover_seq <- seq(min(dat_shrub$shrubCover, na.rm = TRUE),
                 max(dat_shrub$shrubCover, na.rm = TRUE),
                 length.out = 100)

# 2. Create a new data frame for prediction
new_data <- data.frame(
  cover = cover_seq,
  tmean_s = mean(dat_shrub$tmean_s, na.rm = TRUE),
  prcpTempCorr_s = mean(dat_shrub$prcpTempCorr_s, na.rm = TRUE),
  isothermality_s = mean(dat_shrub$isothermality_s, na.rm = TRUE),
  annWatDef_s = mean(dat_shrub$annWatDef_s, na.rm = TRUE),
  prcp_s = mean(dat_shrub$prcp_s, na.rm = TRUE),
  prcp_seasonality_s = mean(dat_shrub$prcp_seasonality_s, na.rm = TRUE),
  prcp_dry_s = mean(dat_shrub$prcp_dry_s, na.rm = TRUE),
  annWetDegDays_s = mean(dat_shrub$annWetDegDays_s, na.rm = TRUE),
  t_warm_s = mean(dat_shrub$t_warm_s, na.rm = TRUE),
  t_cold_s = mean(dat_shrub$t_cold_s, na.rm = TRUE),
  prcp_wet_s = mean(dat_shrub$prcp_wet_s, na.rm = TRUE),
  soilDepth_s= mean(dat_shrub$soilDepth_s, na.rm = TRUE),
  sand_s = mean(dat_shrub$sand_s, na.rm = TRUE),
  coarse_s = mean(dat_shrub$coarse_s, na.rm = TRUE),
  AWHC_s = mean(dat_shrub$AWHC_s, na.rm = TRUE),
  clay_s = mean(dat_shrub$clay_s, na.rm = TRUE),
  carbon_s = mean(dat_shrub$carbon_s, na.rm = TRUE)
)

# 3. Add the cover transformation
new_data$shrubCover_term <- log((best_beta18 + new_data$cover) / (best_beta19 * new_data$cover))

# 4. Predict biomass on the response scale
new_data$predicted_biomass <- predict(final_fit, newdata = new_data, type = "response")

# 5. Plot the predictions

plot(
  new_data$cover,
  new_data$predicted_biomass,
  type = "l",
  lwd = 2,
  col = "blue",
  xlab = "Cover",
  ylab = "Predicted Biomass (Mg/ha)",
  main = "Effect of Cover on Biomass"
)

# plot the predictions and residuals --------------------------------------
# rasterize the biomass and shrub cover data
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") 
# rasterize total biomass in Mg/hect
dat_biomass_rast <- dat %>% 
  terra::vect(geom = c("x", "y"), crs = crs(test_rast)) %>% 
  terra::rasterize(test_rast, field = "totalBiomass_MgHect") %>% 
  terra::aggregate(fun = "mean", na.rm = TRUE, fact = 8)
# rasterize % shrub cover
dat_shrubCover_rast <- dat %>% 
  terra::vect(geom = c("x", "y"), crs = crs(test_rast)) %>% 
  terra::rasterize(test_rast, field = "shrubCover") %>% 
  terra::aggregate(fun = "mean", na.rm = TRUE, fact = 8)

# generate predictions for this model using training data
# the 'shrub cover' term is called "shrubCover_term" in the dat_shrub data.frame
# Predict biomass on the response scale
dat_shrub$predicted_totalBiomass <- predict(final_fit, newdata = dat_shrub, type = "response")
# rasterize predicted total biomass in Mg/hect
dat_biomassPreds_rast <- dat_shrub %>% 
  terra::vect(geom = c("x", "y"), crs = crs(test_rast)) %>% 
  terra::rasterize(test_rast, field = "predicted_totalBiomass") %>% 
  terra::aggregate(fun = "mean", na.rm = TRUE, fact = 8)
plot(dat_biomassPreds_rast)

## calculate the residuals
dat_resids_rast <- dat_biomass_rast - dat_biomassPreds_rast
plot(dat_resids_rast)

# make plots 
(plot_biomass <- ggplot() + 
  geom_spatraster(data = dat_biomass_rast) +
  labs(title = paste0("Observed total biomass from GEDI; Mg/hect")) +
  scale_fill_gradient2(low = "white",
                       high = "purple" , 
                       limits = c(0,2660.6135379),  na.value = "lightgrey") + 
  xlim(c(-2060750, 2555250)) + 
  ylim(c(-1895500, 972500)))

(plot_biomassPreds <- ggplot() + 
    geom_spatraster(data = dat_biomassPreds_rast) +
    labs(title = paste0("Predicted total biomass from model; Mg/hect")) +
    scale_fill_gradient2(low = "white",
                         high = "purple" , 
                         limits = c(0,2660.6135379),  na.value = "lightgrey") + 
    xlim(c(-2060750, 2555250)) + 
    ylim(c(-1895500, 972500)))

# points where values are >500 or < -500
dat_resids_big <- dat_resids_rast %>% 
  terra::mask(mask = ifel(. > 500, 1, NA)) %>% 
  terra::as.points()
dat_resids_small <- dat_resids_rast %>% 
  terra::mask(mask = ifel(. < -500, 1, NA)) %>% 
  terra::as.points()
(plot_biomassResids <- ggplot() + 
    geom_spatraster(data = dat_resids_rast) +
    geom_spatvector(data = dat_resids_big, col = "red") +
    geom_spatvector(data = dat_resids_small, col = "blue") +
    labs(title = paste0("Residuals of biomass predictions (obs - pred)"),
         subtitle = "points show locations where residuals are > 500 (red) or < -500 (blue)") +
    scale_fill_gradient2(low = "blue",
                         high = "red" , 
                         limits = c(-500,500),  na.value = "lightgrey") + 
    xlim(c(-2060750, 2555250)) + 
    ylim(c(-1895500, 972500)))


(plot_shrubCover <- ggplot() + 
    geom_spatraster(data = dat_shrubCover_rast) +
    labs(title = paste0("Modeled shrub cover")) +
    scale_fill_gradient2(low = "white",
                         high = "forestgreen" , 
                         limits = c(0,100),  na.value = "lightgrey") + 
    xlim(c(-2060750, 2555250)) + 
    ylim(c(-1895500, 972500)))


patchwork()