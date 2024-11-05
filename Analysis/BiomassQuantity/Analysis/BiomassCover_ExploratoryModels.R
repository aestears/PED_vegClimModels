#///////////////////
# Models to explore the relationship between biomass and cover
# Alice Stears
# 11/4/24
#///////////////////


# load packages -----------------------------------------------------------
library(tidyverse)
library(interactions)

# load data ---------------------------------------------------------------

# load herbaceous biomass and cover data (from RAP)
herbBio <- readRDS("./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering.rds")

# load tree biomass and cover data
treeBio <- readRDS("./Data_processed/BiomassQuantityData/TreeBiomassCover_withWeatherAndFireFiltering.rds")


# Fit a basic model comparing herb biomass to cover + climate -------------
# look at distribution of response (biomass)
hist(herbBio$HerbaceousBiomass_kgPerHect)
hist(herbBio$TotalHerbaceousCover)
hist(herbBio$tmean_meanAnnAvg_10yr)
hist(herbBio$prcp_meanAnnTotal_10yr)
## simple test model
testMod_herb <- glm(HerbaceousBiomass_kgPerHect ~ TotalHerbaceousCover + TotalHerbaceousCover * tmean_meanAnnAvg_10yr + TotalHerbaceousCover * prcp_meanAnnTotal_10yr, data = herbBio)
coeffs_herb <- testMod_herb$coefficients
summary(testMod_herb)
# look at interaction slopes
int_slopes_tmean <- sim_slopes(testMod_herb, pred = TotalHerbaceousCover, modx = tmean_meanAnnAvg_10yr, data = herbBio)
int_slopes_prcp <- sim_slopes(testMod_herb, pred = TotalHerbaceousCover, modx = prcp_meanAnnTotal_10yr, data = herbBio)
# plot the variation in the relationship between biomass and cover across different values of MAP and MAT
#MAT
tmeanMean <- mean(herbBio$tmean_meanAnnAvg_10yr, na.rm = TRUE)
tmeanSD <- sd(herbBio$tmean_meanAnnAvg_10yr, na.rm = TRUE)
tmeanVals <- round(c(tmeanMean - 1.5*tmeanSD, tmeanMean - tmeanSD, tmeanMean, 
                     tmeanMean + tmeanSD, tmeanMean + 1.5*tmeanSD),2)
#MAP 
prcpMean <- mean(herbBio$prcp_meanAnnTotal_10yr, na.rm = TRUE)
prcpSD <- sd(herbBio$prcp_meanAnnTotal_10yr, na.rm = TRUE)
prcpVals <- round(c(prcpMean - 1.5*prcpSD, prcpMean - prcpSD, prcpMean, prcpMean + prcpSD, prcpMean + 1.5*prcpSD),2)

# predicted data for tmean values
for (i in 1:length(tmeanVals)) {
  temp <- data.frame("TotalHerbaceousCover" = seq(0:100), "prcp_meanAnnTotal_10yr" = prcpMean, 
                     "tmean_meanAnnAvg_10yr" = tmeanVals[i])
  temp2 <- cbind(temp, data.frame("HerbaceousBiomass_kgPerHect" = predict(testMod_herb, temp), "tmeanVal" = tmeanVals[i]))
  if (i == 1) {
    tmeanDat <- temp2
  } else {
    tmeanDat <- rbind(tmeanDat, temp2)
  }
}
# predicted data for precip values
for (i in 1:length(prcpVals)) {
  temp <- data.frame("TotalHerbaceousCover" = seq(0:100), "prcp_meanAnnTotal_10yr" = prcpVals[i], 
                     "tmean_meanAnnAvg_10yr" = tmeanMean)
  temp2 <- cbind(temp, data.frame("HerbaceousBiomass_kgPerHect" = predict(testMod_herb, temp), "prcpVal" = prcpVals[i]))
  if (i == 1) {
    prcpDat <- temp2
  } else {
    prcpDat <- rbind(prcpDat, temp2)
  }
}

#sample dat
sampledDat <- herbBio[sample(c(1:nrow(herbBio)), size = 50000, replace = FALSE),]
# MAT interaction plot
(tempPlot <-
    ggplot() +
    geom_point(data = sampledDat, aes(x = TotalHerbaceousCover, y = HerbaceousBiomass_kgPerHect), alpha = .1)+ 
    geom_line(data = tmeanDat, aes(x = TotalHerbaceousCover, y = HerbaceousBiomass_kgPerHect, 
                                   col = as.factor(tmeanVal), linetype = as.factor(tmeanVal))) + 
    theme_minimal() + 
    scale_color_brewer(type = "seq", palette = "Reds") + 
    guides(color = guide_legend(title = "MAT - prev. 10 years", position = "bottom"), 
           linetype = guide_legend(title = "MAT - prev. 10 years", position = "bottom")) + 
    ggtitle("Model-predicted herb. biomass with \n mean(MAP) and different MAT values")
  # interact_plot(testMod_herb, pred = TotalHerbaceousCover, modx = tmean_meanAnnAvg_10yr, interval = TRUE, 
  #               data = herbBio, modx.values = tmeanVals, legend.main = "MAT-prev. 10 yrs", int.type = "confidence") 
    )
# MAT interaction plot
(prcpPlot <-
    ggplot() +
    geom_point(data = sampledDat, aes(x = TotalHerbaceousCover, y = HerbaceousBiomass_kgPerHect), alpha = .1)+ 
    geom_line(data = prcpDat, aes(x = TotalHerbaceousCover, y = HerbaceousBiomass_kgPerHect, 
                                   col = as.factor(prcpVal), linetype = as.factor(prcpVal))) + 
    theme_minimal() + 
    scale_color_brewer(type = "seq", palette = "Blues") + 
    guides(color = guide_legend(title = "MAP - prev. 10 years", position = "bottom"), 
           linetype = guide_legend(title = "MAP - prev. 10 years", position = "bottom")) + 
    ggtitle("Model-predicted herb. biomass with \n mean(MAT) and different MAP values")
  # interact_plot(testMod_herb, pred = TotalHerbaceousCover, modx = tmean_meanAnnAvg_10yr, interval = TRUE, 
  #               data = herbBio, modx.values = tmeanVals, legend.main = "MAT-prev. 10 yrs", int.type = "confidence") 
)



ggpubr::ggarrange(tempPlot, prcpPlot, nrow = 1)
