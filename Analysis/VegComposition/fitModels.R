#///////////////////
# Fitting models to vegetation composition data
# Alice Stears
# 6/26/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
library(statmod)
library(betareg)
library(glmmTMB)
library(corrplot)
# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- read.csv("./data/DataForAnalysis.csv")
# load meteorological data
dayMet <- read.csv("./data/dayMet/climateValuesForAnalysis_final.csv")

# add met data to veg data ------------------------------------------------
climVar <- dayMet %>% 
  select(year, Long, Lat, totalAnnPrecip, 
         totalAnnSwe, 
         meanAnnTmax, 
         meanAnnTmin, 
         annMaxTmax, annMinTmin, meanAnnVp, tempAmp, swe_meanAnnualAverage, 
         tmin_meanAnnualAverage, tmax_meanAnnualAverage, vp_meanAnnualAverage, 
         prcp_meanAnnualTotal, Start)

# make veg data projection the same as raster data
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
v <- vect(vegDat[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(points_sf))
y <- project(v, crs(test)) 
vegDat[,c("Lon", "Lat")] <- st_coordinates(st_as_sf(y))
plot(vegDat$Lon, vegDat$Lat)

allDat <- vegDat %>% 
  left_join(climVar, by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat"))
  
# plot those that have missing data ... not sure why? there are points that don't have corresponding climate data
good <- allDat[!is.na(allDat$meanAnnTmax),]

bad <- allDat[is.na(allDat$meanAnnTmax),]
## add in the clim data for rounded lat/long points
bad <- bad %>% 
  select(-totalAnnPrecip, 
         -totalAnnSwe, 
         -meanAnnTmax, 
         -meanAnnTmin,
         -annMaxTmax, 
         -annMinTmin, 
         -meanAnnVp, 
         -tempAmp, 
         -swe_meanAnnualAverage, 
         -tmin_meanAnnualAverage, 
         -tmax_meanAnnualAverage, 
         -vp_meanAnnualAverage, 
         -prcp_meanAnnualTotal, 
         -Start) %>% 
  mutate(Lat = round(Lat, 1),
         Lon = round(Lon, 1)) %>% 
  left_join((climVar %>% 
               mutate(Lat = round(Lat, 1),
                      Long = round(Long, 1))), by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat")) %>% 
  unique()
## are still some messed up variables...will need to figure out later
badBad <- bad[!is.na(bad$annMinTmin),]

allDat <- rbind(bad, good)


# double check values ----------------------------------------------------
allDat[allDat$Source == "FIA",]


# Convert % covers to proportions of cover for each class ------------------

allDat$TotalCover <- rowSums(allDat[,c("ShrubCover", "HerbCover", "TotalGramCover", "TotalTreeCover", )])
allDat <- allDat %>% 
  mutate(ShrubProportion = ShrubCover/TotalCover,
         HerbProportion = HerbCover/TotalCover,
         TotalGramProportion = TotalGramCover/TotalCover, 
         C3GramProportion = C3GramCover/TotalCover,
         C4GramProportion = C4GramCover/TotalCover,
         TotalTreeProportion = TotalTreeCover/TotalCover, 
         ConifTreeProportion = ConifTreeCover/TotalCover, 
         AngioTreeProportion = AngioTreeCover/TotalCover
         )
# fit basic models  -------------------------------------------------------

## for these models, I think we want to use a tweedie 
# distribution... ? gamma distribution doesn't allow zeros 
#This paper may be a good option for a more complex modelling approach: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13262
# This is another, multivariate option!? (Damgaard, 2015) in Ecological Informatics
# This is a simpler approach... (Damgaard, 2018) in Journal of Ecology

# tree cover
# get tree data w/ all NAs removed
treeDat <- allDat[!is.na(allDat$TotalTreeCover) & 
                    !is.na(allDat$TotalTreeProportion) &
                    !is.na(allDat$annMinTmin) & 
                    !is.na(allDat$annMaxTmax) &
                    !is.na(allDat$meanAnnTmin) &
                    !is.na(allDat$meanAnnTmax) &
                    !is.na(allDat$totalAnnSwe) & 
                    !is.na(allDat$totalAnnPrecip),]
treeDat$Year <- as.factor(treeDat$Year)

testDat <- allDat %>% 
  filter((TotalTreeCover <= 100 | is.na(TotalTreeCover)) & 
           (TotalGramCover <=100 | is.na(TotalGramCover)) & 
           (HerbCover <=100 | is.na(HerbCover)) & 
           (ShrubCover <=100 | is.na(ShrubCover))) %>% 
  mutate(TotalTreeCover = TotalTreeCover/100,
         TotalGramCover = TotalGramCover/100,
         HerbCover = HerbCover/100,
         ShrubCover = ShrubCover/100)

## make correlation matrix of covariates
corTree <- cor(testDat[,c("annMinTmin", "annMaxTmax", "meanAnnTmin", "meanAnnTmax", "totalAnnSwe", "totalAnnPrecip", "Year")])
corrplot::corrplot(corTree, method = "number")

# convert 0s in tree proportion to .0001
testDat[testDat$TotalTreeCover==0 & !is.na(testDat$TotalTreeCover), "TotalTreeCover"] <- .0001
# convert 1s in tree Cover to .9999
testDat[testDat$TotalTreeCover==1 & !is.na(testDat$TotalTreeCover), "TotalTreeCover"] <- .9999

testDat[testDat$TotalGramCover==0 & !is.na(testDat$TotalGramCover), "TotalGramCover"] <- .0001
# convert 1s in tree Cover to .9999
testDat[testDat$TotalGramCover==1 & !is.na(testDat$TotalGramCover), "TotalGramCover"] <- .9999
# logit transform tree proportion data
testDat$TotalTreeProportion_logit <- car::logit(testDat$TotalTreeProportion, percents = FALSE)

# get 'random' indices to sample the data
randInd <- sample(1:nrow(testDat[!is.na(testDat$TotalTreeCover),]), size = 100000, replace = FALSE)
# tree beta distribution
tree.glm.beta <- betareg(formula = TotalTreeCover ~ totalAnnPrecip + meanAnnTmax + meanAnnTmin + tmax_meanAnnualAverage + prcp_meanAnnualTotal + vp_meanAnnualAverage,
                         data = testDat[!is.na(testDat$TotalTreeCover),][randInd,], link = c("logit"), link.phi = NULL,
                         type = c("ML"))

#betareg:::summary.betareg(tree.glm.beta)
coefficients(tree.glm.beta)
coef(tree.glm.beta)
parameters::p_value(tree.glm.beta)
## plot w/ most significant coefficients (mean annual tmax and total annual precip in the current year)
ggplot(testDat[!is.na(testDat$TotalTreeCover),][randInd,]) + 
  geom_point(aes(totalAnnPrecip, tmax_meanAnnualAverage, col = TotalTreeCover)) + 
  theme_minimal() +
  xlab("Total Precip -- current year (mm)") + 
  ylab("Mean Annual Max. Temp. (C)")


# get 'random' indices to sample the data
randInd <- sample(1:nrow(testDat[!is.na(testDat$TotalGramCover),]), size = 100000, replace = FALSE)
# tree beta distribution
grass.glm.beta <- betareg(formula = TotalGramCover ~ totalAnnPrecip + meanAnnTmax + meanAnnTmin + tmax_meanAnnualAverage + prcp_meanAnnualTotal + vp_meanAnnualAverage,
                         data = testDat[!is.na(testDat$TotalGramCover),][randInd,], link = c("logit"), link.phi = NULL,
                         type = c("ML"))

#betareg:::summary.betareg(tree.glm.beta)
coefficients(grass.glm.beta)
coef(grass.glm.beta)
parameters::p_value(grass.glm.beta)
## plot w/ most significant coefficients (mean annual precip and current year min temp)
ggplot(testDat[!is.na(testDat$TotalGramCover),][randInd,]) + 
  geom_point(aes(prcp_meanAnnualTotal, meanAnnTmin, col = TotalGramCover)) + 
  theme_minimal() +
  xlab("Total Precip. -- current year (mm)") + 
  ylab("Mean Min. Temp. -- current year (C)")

# tree beta distribution w/ allowing a fixed effect of year
tree.glm.beta2 <- betareg(formula = TotalTreeCover ~ 
                            totalAnnPrecip + meanAnnTmax + meanAnnTmin + as.factor(Year),
                          data = testDat[!is.na(testDat$TotalGramCover),], link = c("logit"), link.phi = NULL,
                          type = c("ML"))
coefficients(tree.glm.beta2)

AIC(tree.glm.beta, tree.glm.beta2)

#tree beta distribution w/ allowing phi to vary across years (using glmmTMB)
tree.glm.beta3 <- glmmTMB::glmmTMB(TotalTreeProportion ~ 
                                     totalAnnPrecip + meanAnnTmax + meanAnnTmin + (1|Year)
                                   , data = testDat, family=beta_family(link="logit"))

summary(tree.glm.beta3)


# For comparison this applies a logit-transformation to the empirical proportions and
# then uses a standard linear regression model.
tree.lm.logit<-lm(TotalTreeProportion_logit~totalAnnPrecip + meanAnnTmax + meanAnnTmin,data=testDat)
summary(tree.lm.logit)

#this is code for applying a naive linear model applied to the raw proportions as done
#in the simulation study
tree.lm.raw<-lm(TotalTreeProportion~totalAnnPrecip + meanAnnTmax + meanAnnTmin,data=testDat)
summary(tree.lm.raw)

plot(testDat$TotalTreeProportion, testDat$meanAnnTmax)

# shrub cover
# forb cover
# gram cover


