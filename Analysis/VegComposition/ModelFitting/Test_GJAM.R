#/////////////////// 
# This script tests an approach for estimating vegetation
#cover as a function of climate predictors using Generalized Joint Additive
#Modeling (GJAM) 
# Alice Stears 
# 8/19/24///////////////////


# Load Packages -----------------------------------------------------------

library(tidyverse) 
library(gjam)
library(sf)

# Prepare Data ------------------------------------------------------------

modDat <- readRDS("./data/DataForModels_withSubEcoreg.rds")

## if it's herbs, remove LDC data right now (has some weirdness that's skewing the data... need to figure that out)
if (s == "HerbCover") {
  modDat <- modDat[modDat$Source != "LANDFIRE",]
}

# sub sample for testing vs training data
set.seed(1234)
modDat_1 <- modDat %>% 
  mutate(Lon = st_coordinates(.)[,1], 
         Lat = st_coordinates(.)[,2])  %>% 
  st_drop_geometry() %>% 
  filter(!is.na(newRegion))


pred_vars <- c("swe_meanAnnAvg_30yr", "tmean_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", "PrecipTempCorr_meanAnnAvg_30yr", "isothermality_meanAnnAvg_30yr", "annWetDegDays_meanAnnAvg_30yr")
# removed VPD, since it's highly correlated w/ tmean and prcp

names(pred_vars) <- pred_vars

modDat$LitterCover_dec <- modDat$LitterCover/100
## convert % cover into fractional cover (So they sum to one)
modDat$TotalCover_dec <- base::rowSums(modDat[,c("ShrubCover_dec", "HerbCover_dec", 
                                                 "TotalGramCover_dec", 
                                                 "TotalTreeCover_dec", 
                                                 "LitterCover_dec"
                                                         )] %>% st_drop_geometry())

# calculate cover in terms of 'fraction' (rather than the 'dec' version, which can sum to >1)
modDat_temp <- 
  modDat %>% 
  mutate(ShrubCover_frac = ShrubCover_dec/TotalCover_dec, 
         HerbCover_frac = HerbCover_dec/TotalCover_dec, 
         TotalGramCover_frac = TotalGramCover_dec/TotalCover_dec, 
         TotalTreeCover_frac = TotalTreeCover_dec/TotalCover_dec, 
         LitterCover_frac = LitterCover_dec/TotalCover_dec)

## remove rows for data that have NAs in the predictors (lag starts before range of DayMet data)

modDat_1 <- modDat_temp %>% 
  st_drop_geometry() %>% 
  drop_na(swe_meanAnnAvg_30yr, tmean_meanAnnAvg_30yr, prcp_meanAnnTotal_30yr, 
          precip_Seasonality_meanAnnAvg_30yr, PrecipTempCorr_meanAnnAvg_30yr, 
          isothermality_meanAnnAvg_30yr, TotalCover_dec
          ) %>% 
  select(swe_meanAnnAvg_30yr, tmean_meanAnnAvg_30yr, prcp_meanAnnTotal_30yr, 
         precip_Seasonality_meanAnnAvg_30yr, PrecipTempCorr_meanAnnAvg_30yr, 
         isothermality_meanAnnAvg_30yr, ShrubCover_frac, HerbCover_frac, TotalGramCover_frac, 
         TotalTreeCover_frac)


df_sample <- {
  reordered <- slice_sample(modDat_1, prop = 1)
  sample_group <- 1
  n_train <- 15000
  low <- (sample_group - 1)*n_train + 1 # first row (of reordered data) to get
  high <- low + n_train - 1 # last row
  if(high > nrow(modDat_1)) {
    warning(paste0("n_train size due to low sample size (i.e. < ", n_train,")"))
    n_train <- n_train*.8
    low <- (sample_group - 1)*n_train + 1 # first row (of reordered data) to get
    high <- low + n_train - 1 # last row
    #stop('trying to sample from rows that dont exist')
  }
  reordered[low:high, ]
  
}
  n_test <- 500
df_test <-  
  modDat_1 %>% 
    anti_join(df_sample) %>% 
    slice_sample(n = n_test)


# small sample for certain plots
df_small <- slice_sample(modDat_1, n = 500)

# Define gjam inputs ---------------------------------------------------
# based on this tutorial (https://rpubs.com/clanescher/gjam)
# add row names to df
rownames(df_sample) <- paste0("gridCellNum_", seq(1:nrow(df_sample)))

# response data
ydata <- df_sample %>% 
  select(ShrubCover_frac:TotalTreeCover_frac) 
names(ydata) <- c("ShrubCover", "HerbCover", "TotalGramCover", 
                  "TotalTreeCover")

# predictor data
xdata <- df_sample %>% 
  select(swe_meanAnnAvg_30yr:isothermality_meanAnnAvg_30yr)
names(xdata) <- c("sweMeanAnnAvg30yr"               ,
                  "tmeanMeanAnnAvg30yr"             ,
                  "prcpMeanAnnTotal30yr"            ,
                  "precipSeasonalityMeanAnnAvg30yr",
                  "PrecipTempCorrMeanAnnAvg30yr"    ,
                  "isothermalityMeanAnnAvg30yr"  )
# effort 
edata <- list(columns = 1:ncol(ydata), 
              values = rep(1,ncol(ydata)))


# Set up the model --------------------------------------------------------
  
formula <- as.formula(~sweMeanAnnAvg30yr+ tmeanMeanAnnAvg30yr+ prcpMeanAnnTotal30yr+
                      precipSeasonalityMeanAnnAvg30yr+ PrecipTempCorrMeanAnnAvg30yr+ 
                      isothermalityMeanAnnAvg30yr)

# I don't think we should need to do dimension reduction, since we have high S and low n

# set up model priors: 
# want to have uninformative priors to start with 
prior <- gjamPriorTemplate(formula = formula, 
                           xdata = xdata, ydata = ydata, lo = -Inf, hi = Inf)


# make the model list
mlist <- list(ng=5000, # not sure if this is good
              burnin = 1000,  # not sure if this is good
              typeNames = 'FC', 
              betaPrior = prior, 
              effort = edata)


# run the model!  ---------------------------------------------------------

out <- gjam(formula = formula, xdata = xdata, ydata = ydata, modelList = mlist)
