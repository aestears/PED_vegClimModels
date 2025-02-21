#///////////////////
# Preparing dataset for ecoregion classification models
# Alice Stears
# 12/12/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

# load dayMet climate data that has been processed in '03_GettingWeatherData.R'
climDat <- readRDS("./Data_processed/CoverData/dayMet_intermediate/ECOREGIONSclimVars_AnnualMeansAndLaggedValues.rds")

# load ecoregion data
regions <- sf::st_read(dsn = "./Data_raw/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1")


soilRast <- readRDS("./Data_processed/SoilsRaster.rds")

# add ecoregion classification to climate data  ---------------------------

climDat_use <- climDat[climDat$year == 2011,]
rm(climDat)
gc()

# make sure crs of climate and ecoregion data are the same 
# want them both to have this same crs as below
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

regions_2 <- st_transform(regions, crs = st_crs(test_rast)) %>% 
  st_make_valid()
crs(test_rast) == crs(regions_2)
# transform modDat to sf to later use st_join
climDat_2 <- st_as_sf(climDat_use, coords = c("Long", "Lat"))
climDat_2 <- st_set_crs(climDat_2, value = st_crs(test_rast))

crs(regions_2) == crs(climDat_2)

# match the ecoregion to point data ---------------------------------------
climDat_3 <- sf::st_join(climDat_2, regions_2)

# group into coarser ecoregions -------------------------------------------
# make a lookup table
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(climDat_3$NA_L1NAME)), 
                        "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
                                        "dryShrubGrass", "dryShrubGrass", "eastForest",
                                        "westForest", "dryShrubGrass", "westForest", 
                                        "eastForest", NA
                        ))
# ggplot(climDat_3) + 
#   geom_sf(aes(col = NA_L1NAME))



# add to main data.frame 
newDat <- climDat_3 %>% 
  left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion)) 


# add soils data ----------------------------------------------------------
# sample raster to get values for the points in the cover dataset

newDat_df <- soilRast %>% 
  terra::extract(y = newDat, xy = TRUE, bind = TRUE) %>% 
  as.data.frame() 

vegSoils_new <- 
  newDat_df %>% 
  dplyr::mutate(
    # Soil depth 
    soilDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                             "horizonThickness_cm_176cm")], sum, na.rm = TRUE),
    #Surface clay (influences how much moisture can get into the profile)
    surfaceClay_perc = clayPerc_2cm) %>% 
  mutate(soilDepth = replace(soilDepth, is.na(horizonThickness_cm_2cm), values = NA)) %>% 
  mutate( 
    # Sand average across depths (avg. weighted by width of layer)
    avgSandPerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                           "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                           "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                           "horizonThickness_cm_176cm", "sandPerc_2cm", "sandPerc_7cm" , "sandPerc_15cm",
                                           "sandPerc_25cm" , "sandPerc_35cm", "sandPerc_50cm" , "sandPerc_70cm", "sandPerc_90cm" ,
                                           "sandPerc_125cm", "sandPerc_176cm", "soilDepth")], 
                                       function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                horizonThickness_cm_176cm, sandPerc_2cm, sandPerc_7cm , sandPerc_15cm,
                                                sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                                                sandPerc_125cm,sandPerc_176cm, soilDepth) {
                                         y <- sum(c(sandPerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                    sandPerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                    sandPerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                    sandPerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                    sandPerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                    sandPerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                    sandPerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                    sandPerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                    sandPerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                    sandPerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                  na.rm = TRUE)/1 
                                         # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                         return(y)
                                       }
    ),
    # Coarse fragments average across depths (avg. weighted by width of layer)
    avgCoarsePerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                             "horizonThickness_cm_176cm", "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm",
                                             "coarsePerc_25cm" , "coarsePerc_35cm", "coarsePerc_50cm" , "coarsePerc_70cm", "coarsePerc_90cm" ,
                                             "coarsePerc_125cm","coarsePerc_176cm", "soilDepth")], 
                                         function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                  horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                  horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                  horizonThickness_cm_176cm, coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                                  coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                                  coarsePerc_125cm,coarsePerc_176cm, soilDepth) {
                                           y <- sum(c(coarsePerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                      coarsePerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                      coarsePerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                      coarsePerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                      coarsePerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                      coarsePerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                      coarsePerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                      coarsePerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                      coarsePerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                      coarsePerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                    na.rm = TRUE)/1 
                                           # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                           return(y)
                                         }
    ), 
    # soil organic carbon average across depths (avg. weighted by width of layer)
    avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm
                
  )




# # total profile available water-holding capacity
temp <- vegSoils_new %>% 
  mutate(clayPerc_2cm = clayPerc_2cm/100,
         clayPerc_7cm = clayPerc_7cm/100,
         clayPerc_15cm = clayPerc_15cm/100,
         clayPerc_25cm = clayPerc_25cm/100,
         clayPerc_35cm = clayPerc_35cm/100,
         clayPerc_50cm = clayPerc_50cm/100,
         clayPerc_70cm = clayPerc_70cm/100,
         clayPerc_90cm = clayPerc_90cm/100,
         clayPerc_125cm = clayPerc_125cm/100,
         clayPerc_176cm = clayPerc_176cm/100,
         sandPerc_2cm = sandPerc_2cm/100,
         sandPerc_7cm = sandPerc_7cm/100,
         sandPerc_15cm = sandPerc_15cm/100,
         sandPerc_25cm = sandPerc_25cm/100,
         sandPerc_35cm = sandPerc_35cm/100,
         sandPerc_50cm = sandPerc_50cm/100,
         sandPerc_70cm = sandPerc_70cm/100,
         sandPerc_90cm = sandPerc_90cm/100,
         sandPerc_125cm = sandPerc_125cm/100,
         sandPerc_176cm = sandPerc_176cm/100,
         coarsePerc_2cm = coarsePerc_2cm/100,
         coarsePerc_7cm = coarsePerc_7cm/100,
         coarsePerc_15cm = coarsePerc_15cm/100,
         coarsePerc_25cm = coarsePerc_25cm/100,
         coarsePerc_35cm = coarsePerc_35cm/100,
         coarsePerc_50cm = coarsePerc_50cm/100,
         coarsePerc_70cm = coarsePerc_70cm/100,
         coarsePerc_90cm = coarsePerc_90cm/100,
         coarsePerc_125cm = coarsePerc_125cm/100,
         coarsePerc_176cm = coarsePerc_176cm/100) #%>% 
#slice(1:3) 
# calculate # # intermediate value 'p' 
vegSoil_p <- pmap(.l = temp[,c("sandPerc_2cm", "sandPerc_7cm", "sandPerc_15cm", 
                               "sandPerc_25cm", "sandPerc_35cm", "sandPerc_50cm", 
                               "sandPerc_70cm", "sandPerc_90cm" ,"sandPerc_125cm", 
                               "sandPerc_176cm",
                               "clayPerc_2cm", "clayPerc_7cm" , "clayPerc_15cm", 
                               "clayPerc_25cm", "clayPerc_35cm", "clayPerc_50cm", 
                               "clayPerc_70cm", "clayPerc_90cm" ,"clayPerc_125cm", 
                               "clayPerc_176cm",
                               "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm", 
                               "coarsePerc_25cm", "coarsePerc_35cm", "coarsePerc_50cm", 
                               "coarsePerc_70cm", "coarsePerc_90cm" ,"coarsePerc_125cm", 
                               "coarsePerc_176cm")], 
                  function (sandPerc_2cm, sandPerc_7cm, sandPerc_15cm, 
                            sandPerc_25cm, sandPerc_35cm, sandPerc_50cm, 
                            sandPerc_70cm, sandPerc_90cm ,sandPerc_125cm, 
                            sandPerc_176cm,
                            clayPerc_2cm, clayPerc_7cm , clayPerc_15cm, 
                            clayPerc_25cm, clayPerc_35cm, clayPerc_50cm, 
                            clayPerc_70cm, clayPerc_90cm ,clayPerc_125cm, 
                            clayPerc_176cm,
                            coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm, 
                            coarsePerc_25cm, coarsePerc_35cm, coarsePerc_50cm, 
                            coarsePerc_70cm, coarsePerc_90cm ,coarsePerc_125cm, 
                            coarsePerc_176cm) {
                    p <- rSOILWAT2::ptf_estimate(
                      sand = c(sandPerc_2cm,sandPerc_7cm , sandPerc_15cm,
                               sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                               sandPerc_125cm,sandPerc_176cm),
                      clay = c(clayPerc_2cm,clayPerc_7cm , clayPerc_15cm,
                               clayPerc_25cm , clayPerc_35cm, clayPerc_50cm , clayPerc_70cm, clayPerc_90cm ,
                               clayPerc_125cm,clayPerc_176cm),
                      fcoarse = c(coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                  coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                  coarsePerc_125cm,coarsePerc_176cm),
                      swrc_name = "Campbell1974",
                      ptf_name = "Cosby1984"
                    )
                  }
)

# calculate intermediate value 'tmp'
# reference "temp" data frame (which has the raw soil variables), as well as vegSoil_p, a list which has matrices for p calculated above
vegSoil_tmp <- map(.x = c(1:nrow(temp)), 
                   function (n) {
                     tmp <- rSOILWAT2::swrc_swp_to_vwc(
                       c(-1.5, -0.033), ##AES should I change this? not totally clear what these values indicate 
                       fcoarse = unlist(as.vector(temp[n,c("coarsePerc_2cm" ,                           
                                                           "coarsePerc_7cm" ,  "coarsePerc_15cm",                        
                                                           "coarsePerc_25cm",  "coarsePerc_35cm",                        
                                                           "coarsePerc_50cm",  "coarsePerc_70cm",                        
                                                           "coarsePerc_90cm",  "coarsePerc_125cm",                        
                                                           "coarsePerc_176cm")])),
                       swrc = list(name = "Campbell1974", swrcp = vegSoil_p[[n]])
                     )
                   }
)


#   # calculate final value 'awc' 
vegSoil_awc <- map(.x = c(1:nrow(temp)), 
                   function (n) {
                     awc <- temp[n,c("horizonThickness_cm_2cm"  ,                 
                                     "horizonThickness_cm_7cm"  ,                  "horizonThickness_cm_15cm"    ,              
                                     "horizonThickness_cm_25cm" ,                  "horizonThickness_cm_35cm"    ,              
                                     "horizonThickness_cm_50cm" ,                  "horizonThickness_cm_70cm"    ,              
                                     "horizonThickness_cm_90cm" ,                  "horizonThickness_cm_125cm"   ,              
                                     "horizonThickness_cm_176cm")] * as.vector(diff(vegSoil_tmp[[n]])
                                     )
                     #AES I assume that I sum these values across the entire profile to get "total profile awc"??
                     totAWC <- sum(awc, na.rm = TRUE)
                   }
) 

vegSoils_new$totalAvailableWaterHoldingCapacity <- unlist(vegSoil_awc)


# code from Daniel to calculate AWC
#Total profile available water-holding capacity (may be some rSOILWAT functions that can help with this? 
# Available water holding capacity AWC
#   soils <- rSOILWAT2::swSoils_Layers(rSOILWAT2::sw_exampleData)
# 
# p <- rSOILWAT2::ptf_estimate(
#   sand = soils[, sand_frac],
#   clay = soils[, clay_frac],
#   fcoarse = soils[, gravel_content],
#   swrc_name = Campbell1974,
#   ptf_name = Cosby1984
# )
# tmp <- rSOILWAT2::swrc_swp_to_vwc(
#   c(-1.5, -0.033),
#   fcoarse = soils[, gravel_content],
#   swrc = list(name = Campbell1974, swrcp = p)
# )
# awc <- diff(c(0, soils[, depth_cm])) * as.vector(diff(tmp))

# remove unnecessary soils variables 
vegSoils_final <- vegSoils_new %>% 
  dplyr::select(-c(clayPerc_2cm:organicCarbonPerc_176cm)) 


# Save Data for further analysis ------------------------------------------
saveRDS(vegSoils_final, "./Data_processed/EcoRegion_climSoilData.rds")

