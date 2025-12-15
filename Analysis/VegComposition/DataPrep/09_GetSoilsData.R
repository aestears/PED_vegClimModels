#///////////////////
# Acquiring and adding soils data to cover data
# Alice Stears
# 11/5/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(RNetCDF)
library(terra)
library(sf)
library(rSOILWAT2)

# read in veg data --------------------------------------------------------
vegDat_temp <- readRDS("./Data_processed/CoverData/DataForModels_withEcoregion_sampledRAP_LF.rds")
# should change the geometry to points (was small polygons for spatial joins) 

vegDat <- vegDat_temp %>% 
  st_centroid()

vegDat %>% 
  ggplot() + 
  facet_wrap(~Year) + 
  geom_point(aes(x = Long, y = Lat))


# read in soils data ------------------------------------------------------
# this dataset is from Daniel Schlaepfer, who provided the following information:
# They are based on SOLUS100 data by Travis Nauman et al.
# Data: https://agdatacommons.nal.usda.gov/articles/dataset/Data_from_Soil_Landscapes_of_the_United_States_100-meter_SOLUS100_soil_property_maps_project_repository/25033856
# Article: in revision
# 
# Attached files were calculated from the original 100-m at specific point depths with the following parameters:
#   * aggregation 100-m to 4-km grid: average
# * soil layers: 0-3, 10, 20, 30, 40, 60, 80, 100, 150, 201 cm
# * soil depth: resdept (i.e., bedrock)
# * deepest layer:
#   if (resdept >= 5 cm next shallow standard layer depth) then new layer
# * conversion point depths to soil layers: trapezoidal rule
# * extrapolation from 150-cm point depth to deeper layers: 150-cm values

if (sum(list.files("./Data_processed/") == "SoilsRaster.rds") == 0) {
  # if there is NOT an .rds file called SoilsRaster, do the following
  # load gridded soils data from Daniel (currently an old version, will be updated w/ SOLUS100 data)
  gridClay <- terra::rast(x = "./Data_raw/soilsDB_new/claytotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridClay) <- c(clayPerc_2cm,
           "clayPerc_7cm"  ,            
           "clayPerc_15cm" , 
           "clayPerc_25cm", 
           "clayPerc_35cm" ,
           "clayPerc_50cm" , 
           "clayPerc_70cm" , 
           "clayPerc_90cm" , 
           "clayPerc_125cm", 
           "clayPerc_176cm") 
  
  gridSand <- terra::rast(x = "./Data_raw/soilsDB_new/sandtotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat) )
  names(gridSand) <- c("sandPerc_2cm" ,         
            "sandPerc_7cm",           
           "sandPerc_15cm" ,        
           "sandPerc_25cm",         
           "sandPerc_35cm",
           "sandPerc_50cm",   
           "sandPerc_70cm" ,          
           "sandPerc_90cm",          
          "sandPerc_125cm",
          "sandPerc_176cm")
  
  gridSilt <- terra::rast(x = "./Data_raw/soilsDB_new/silttotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat)) 
  names(gridSilt) <- c(
            "siltPerc_2cm",
            "siltPerc_7cm",
           "siltPerc_15cm",
           "siltPerc_25cm",
           "siltPerc_35cm",
           "siltPerc_50cm",
           "siltPerc_70cm",
           "siltPerc_90cm",
          "siltPerc_125cm",
          "siltPerc_176cm")

    gridDensity <- terra::rast(x = "./Data_raw/soilsDB_new/dbovendry_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
    names(gridDensity) <- c(
           "density_gcm3_2cm" ,
           "density_gcm3_7cm",
           "density_gcm3_15cm",
           "density_gcm3_25cm",
           "density_gcm3_35cm"  ,
           "density_gcm3_50cm",
           "density_gcm3_70cm",
           "density_gcm3_90cm",
           "density_gcm3_125cm",
           "density_gcm3_176cm")
  
    gridThickness <- terra::rast(x = "./Data_raw/soilsDB_new/hzthk_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat)) 
    names(gridThickness) <- c(
           "horizonThickness_cm_2cm" , 
           "horizonThickness_cm_7cm",
           "horizonThickness_cm_15cm", 
           "horizonThickness_cm_25cm", 
           "horizonThickness_cm_35cm", 
           "horizonThickness_cm_50cm", 
           "horizonThickness_cm_70cm", 
           "horizonThickness_cm_90cm",
           "horizonThickness_cm_125cm" ,
           "horizonThickness_cm_176cm")
    
  gridCoarse <- terra::rast(x = "./Data_raw/soilsDB_new/fragvol_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridCoarse) <- c(
           "coarsePerc_2cm" ,
           "coarsePerc_7cm",
           "coarsePerc_15cm",
           "coarsePerc_25cm",
           "coarsePerc_35cm",
           "coarsePerc_50cm",
           "coarsePerc_70cm",
           "coarsePerc_90cm",
           "coarsePerc_125cm",
           "coarsePerc_176cm")
  
  gridCarbon <- terra::rast(x = "./Data_raw/soilsDB_new/soc_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridCarbon) <- c(
    "organicCarbonPerc_2cm",
           "organicCarbonPerc_7cm",
           "organicCarbonPerc_15cm" ,
           "organicCarbonPerc_25cm" ,
           "organicCarbonPerc_35cm" ,
           "organicCarbonPerc_50cm" ,
           "organicCarbonPerc_70cm" ,
           "organicCarbonPerc_90cm" ,
           "organicCarbonPerc_125cm" ,
           "organicCarbonPerc_176cm")
  
  # stack into a single raster
  soilRast <- c(gridClay, gridSand, gridSilt, gridCoarse, gridDensity, gridThickness, gridCarbon)
  saveRDS(soilRast, file = "./Data_processed/SoilsRaster.rds")
} else {
  # otherwise, read in the file
  soilRast <- readRDS("./Data_processed/SoilsRaster.rds")
}


# sample soils data for veg. points ---------------------------------------
# sample raster to get values for the points in the cover dataset

vegSoils_df <- soilRast %>% 
  terra::extract(y = vegDat #%>% dplyr::select(-x,-y)
                 , xy = TRUE, bind = TRUE) %>% 
  as.data.frame()


# calculate soils variables w/ cover data ---------------------------------
names(vegSoils_df)[c(length(names(vegSoils_df))-1, length(names(vegSoils_df)))] <- c("x_UTM", "y_UTM")
vegSoils_new <- 
  vegSoils_df %>% 
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
    #   pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
    #                                          "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
    #                                          "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
    #                                          "horizonThickness_cm_176cm", "organicCarbonPerc_2cm", "organicCarbonPerc_7cm" , "organicCarbonPerc_15cm",
    #                                          "organicCarbonPerc_25cm" , "organicCarbonPerc_35cm", "organicCarbonPerc_50cm" , "organicCarbonPerc_70cm", "organicCarbonPerc_90cm" ,
    #                                          "organicCarbonPerc_125cm","organicCarbonPerc_176cm", "soilDepth")], 
    #                                      function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
    #                                               horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
    #                                               horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
    #                                               horizonThickness_cm_176cm, organicCarbonPerc_2cm, organicCarbonPerc_7cm , organicCarbonPerc_15cm,
    #                                               organicCarbonPerc_25cm , organicCarbonPerc_35cm, organicCarbonPerc_50cm , organicCarbonPerc_70cm, organicCarbonPerc_90cm ,
    #                                               organicCarbonPerc_125cm,organicCarbonPerc_176cm, soilDepth) {
    #                                        y <- sum(c(organicCarbonPerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
    #                                                   organicCarbonPerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
    #                                                   organicCarbonPerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
    #                                                   organicCarbonPerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
    #                                                   organicCarbonPerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
    #                                                   organicCarbonPerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
    #                                                   organicCarbonPerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
    #                                                   organicCarbonPerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
    #                                                   organicCarbonPerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
    #                                                   organicCarbonPerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
    #                                                 na.rm = TRUE)/1 
    #                                        # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
    #                                        return(y)
    #                                      }
    # )                     
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
    select(-c(clayPerc_2cm:organicCarbonPerc_176cm))
  
  
  ## drop data points for which there are no climate variables (prior to 2000)
  vegSoils_final <- vegSoils_final %>% 
    filter(!is.na(durationFrostFreeDays_meanAnnAvg_3yrAnom))
 
  # prepare final dataset
  # give each row a unique ID
  vegSoils_final <- 
    vegSoils_final %>% 
    mutate(uniqueID = seq(1:nrow(.)))
  
  # give coordinates
  test <- vegSoils_final %>% 
    #slice_sample(n = 1000) %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = st_crs(vegDat)) #%>% 
    #st_drop_geometry() %>% 
    #st_buffer(50) %>% 
     # sf::st_join(CAMcoverDat %>% select(CAMCover, Year.x, geometry) %>% st_buffer(100), by = c("Year" = "Year.x"))
  
  #test <- test %>% 
    #filter(Year == Year.x)
  uniqueID_dups <- duplicated(test$uniqueID)
  test <- test[!uniqueID_dups,]
  
  newFinalDat <- vegSoils_final #%>% 
    #dplyr::left_join(test %>% st_drop_geometry() %>% select(CAMCover, uniqueID), by = "uniqueID")
  
  
  # save data  --------------------------------------------------------------

  # newFinalDat[newFinalDat$precip_driestMonth_meanAnnAvg_3yr == newFinalDat$precip_driestMonth_meanAnnAvg_CLIM, 
  #            c("precip_driestMonth_meanAnnAvg_3yrAnom")
  #           ] <- 0
  
  saveRDS(newFinalDat, 
          "./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf_sampledRAP_LF.rds")
  #vegSoils_final <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")

  
 # plot(newFinalDat$tmean, 
 #      newFinalDat$tmin_annAvg)
 # 
 # newFinalDat %>% 
 #   filter(!is.na(TotalHerbaceousCover)) %>% 
 #   ggplot() + 
 #   facet_wrap(~Year) + 
 #   geom_point(aes(x = Long, y = Lat))

   
  