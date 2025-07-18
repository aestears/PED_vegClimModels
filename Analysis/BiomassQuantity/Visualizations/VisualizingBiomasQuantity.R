#///////////////////
# Visualizing raw cover data from sources
# Alice Stears
# 11/24/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(ggpubr)
library(tidyterra)
library(ggpubr)

# load data ---------------------------------------------------------------

# Get total biomass data from Spawn paper ---------------------------------
spawnBiomass <- terra::rast("./Data_processed/BiomassQuantityData/Spawn2020_biomassRaster.tif") 
# reproject the points to match the crs of the biomass data 

# Get total biomass data from GEDI ----------------------------------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# load raw data
GEDIBiomass <- terra::rast("./Data_processed/BiomassQuantityData/GEDI_biomassRaster.tif") 

# Read in RAP biomass data ------------------------------------------------
# units: Mg/hectare
RAPbiomass <- terra::rast("./Data_processed/BiomassQuantityData/RAP_biomassRaster_totalHerb_Mg_Hect_averagedfrom2019to2021.tif") 
# average from 2010 to 2020
#RAPbiomass_avg <- mean(RAPbiomass$year_2010, RAPbiomass$year_2011, RAPbiomass$year_2012, RAPbiomass$year_2013, RAPbiomass$year_2014, RAPbiomass$year_2015, RAPbiomass$year_2016, RAPbiomass$year_2017, RAPbiomass$year_2018, RAPbiomass$year_2019, RAPbiomass$year_2020)
RAPbiomass_avg <- RAPbiomass

# Read in FIA biomass data ------------------------------------------------
FIAbiomass <- terra::rast("./Data_processed/BiomassQuantityData/FIA_biomassRaster.tif")
# tree live biomass in Mg/hectare

# aggregate all of the rasters for comparison -----------------------------
FIAbiomass_trim <- FIAbiomass %>% 
  terra::aggregate(4, na.rm = TRUE, fun = "mean") %>% 
  terra::crop(ext(-2560750, 3053250, -2091500, 1584500))
(FIAbiomass_plot <- 
    ggplot() + 
    geom_spatraster(data = FIAbiomass_trim, aes(fill = JENK_LIVE)) + 
    ggtitle("Biomass from FIA", subtitle = "Tree live biomass sampled from 2009 to 2019, Mg /ha"))

spawnBiomass_trim <- spawnBiomass %>% 
  terra::aggregate(4, na.rm = TRUE, fun = "mean") %>% 
  terra::crop(ext(-2560750, 3053250, -2091500, 1584500))
(spawnBiomass_plot <- ggplot() + 
    geom_spatraster(data = spawnBiomass_trim$aboveground_biomass_hacked) + 
    ggtitle("Biomass from Spawn et al., 2020", subtitle = "Aboveground living biomass in 2010 (carbon * 2) Mg/ha"))

RAPbiomass_trim <- RAPbiomass_avg %>%
  terra::aggregate(4, na.rm = TRUE, fun = "mean") %>% 
  terra::crop(ext(-2560750, 3053250, -2091500, 1584500))
(RAPbiomass_plot <- ggplot() + 
    geom_spatraster(data = RAPbiomass_trim) + 
    ggtitle("Biomass from RAP", subtitle = "Aboveground biomass, averaged from 2010 to 2020, Mg /ha"))

GEDIBiomass_trim <- GEDIBiomass %>% 
  terra::aggregate(4, na.rm = TRUE, fun = "mean") %>% 
  terra::crop(ext(-2560750, 3053250, -2091500, 1584500))
(GEDIBiomass_plot <- ggplot() + 
    geom_spatraster(data = GEDIBiomass_trim) + 
    ggtitle("Biomass from GEDI", subtitle = "Mean aboveground biomass density 2019-2023, Mg /ha"))

# Difference between Spawn and RAP + FIA ----------------------------------
spawnBiomass_minus_RAPandFIA <- (spawnBiomass_trim$aboveground_biomass_hacked) - RAPbiomass_trim - FIAbiomass_trim$JENK_LIVE
# plot 
(leftoverBiomass_Spawn <- 
    ggplot() + 
    geom_spatraster(data = spawnBiomass_minus_RAPandFIA %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("Biomass from Spawn et al. - RAP biomass - FIA biomass", subtitle = "Mg /ha") + 
    scale_fill_gradient2(low = "red",
                         mid = "white" ,
                         high = "blue" , 
                         midpoint = 0, #limits = c( -407,235),  
                         na.value = "lightgrey") )

# Difference between GEDI and RAP + FIA ----------------------------------
GEDIbiomass_minus_RAPandFIA <- (GEDIBiomass_trim) - RAPbiomass_trim - FIAbiomass_trim$JENK_LIVE
# plot 
(leftoverBiomass_GEDI <- 
    ggplot() + 
    geom_spatraster(data = GEDIbiomass_minus_RAPandFIA %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("Biomass from GEDI - RAP biomass - FIA biomass", subtitle = "Mg /ha") + 
    scale_fill_gradient2(low = "red",
                         mid = "white" ,
                         high = "blue" , 
                         midpoint = 0, #limits = c( -407,235),  
                         na.value = "lightgrey") )


# Compare GEDI and spawn
GEDIdifferenceSpawn <- GEDIBiomass_trim- spawnBiomass_trim$aboveground_biomass_hacked

(totalBiomassDifference <- 
    ggplot() + 
    geom_spatraster(data = GEDIdifferenceSpawn %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("GEDI - Spawn", subtitle = "Total biomass in Mg /ha") + 
    scale_fill_gradient2(low = "red",
                         mid = "white" ,
                         high = "blue" , 
                         midpoint = 0, #limits = c( -407,235),  
                         na.value = "lightgrey") )

# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/soilsFigDatFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
ggarrange(spawnBiomass_plot, 
          RAPbiomass_plot, 
          FIAbiomass_plot, leftoverBiomass_Spawn,
          ncol = 1, 
          nrow = 4) %>% 
  ggexport(
    filename = "./Figures/BiomassExplorationFigures_compareToSpawn.pdf", 
    width = 9, height = 25)

    ggarrange(GEDIBiomass_plot, 
          RAPbiomass_plot, 
          FIAbiomass_plot, leftoverBiomass_GEDI,
          ncol = 1, 
          nrow = 4) %>% 
  ggexport(
    filename = "./Figures/BiomassExplorationFigures_compareToGEDI.pdf", 
    width = 9, height = 25)
    

# Comparing biomass as percentages ---------------------------------------
# % difference between Spawn and RAP + FIA
    percDiff_spawnBiomass_minus_RAPandFIA <- (((spawnBiomass_trim$aboveground_biomass_hacked) - RAPbiomass_trim - FIAbiomass_trim$JENK_LIVE)/spawnBiomass_trim$aboveground_biomass_hacked)*100
    extremePerc_spawn <- percDiff_spawnBiomass_minus_RAPandFIA< -5000  
    extremePerc_spawnPoints <- terra::as.points(extremePerc_spawn, values = TRUE) %>% 
      sf::st_as_sf() %>% 
      filter(aboveground_biomass_hacked == 1)
    # plot 
    (leftoverBiomass_Spawn_percDiff <- 
        ggplot() + 
        geom_spatraster(data = percDiff_spawnBiomass_minus_RAPandFIA %>% 
                          terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
       # geom_sf(data = extremePerc_spawnPoints) +
        ggtitle("Percent difference: (Biomass from Spawn et al. - RAP biomass - FIA biomass)/SpawnBiomass*100"#, 
                #subtitle = "Black points = % difference less than -5000%"
                ) + 
        scale_fill_gradient2(low = "green",
                             #mid = "white" ,
                             high = "orange" ,limits = c(-100,100),  
                             na.value = "lightgrey") )
    
    
    # % difference between GEDI and RAP + FIA
    percDiff_GEDIBiomass_minus_RAPandFIA <- (((GEDIBiomass_trim) - RAPbiomass_trim - FIAbiomass_trim$JENK_LIVE)/GEDIBiomass_trim)*100
    extremePerc_GEDI <- percDiff_GEDIBiomass_minus_RAPandFIA< -5000  
    extremePerc_GEDIPoints <- terra::as.points(extremePerc_GEDI, values = TRUE) %>% 
      sf::st_as_sf() %>% 
      filter(GEDI04_B_MW019MW223_02_002_02_R01000M_MU == 1)
    # plot 
    (leftoverBiomass_GEDI_percDiff <- 
        ggplot() + 
        geom_spatraster(data = percDiff_GEDIBiomass_minus_RAPandFIA %>% 
                          terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
        #geom_sf(data = extremePerc_GEDIPoints) +
        ggtitle("Percent difference: (Biomass from GEDI. - RAP biomass - FIA biomass)/GEDIBiomass*100"#,subtitle = "Black points = % difference less than -5000&"
                ) + 
        scale_fill_gradient2(low = "green",
                             #mid = "white" ,
                             high = "orange" ,limits = c(-100,100),  
                             na.value = "lightgrey") )
    
    