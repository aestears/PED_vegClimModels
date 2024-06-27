#///////////////////
# Getting remotely sensed c3/c4 data and preparing for analysis
# Alice Stears
# 6/20/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)


# read in data  -----------------------------------------------------------
# this is data from the NACP SYNMAP North American Grassland C3 and C4 maps 0.25 degree 
# (remotely sensed map of c3/c4 ratios in grasslands) https://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=20043
# paper: https://openknowledge.nau.edu/id/eprint/701/7/Wei_Y_etal_2014_North_American_Carbon_Program(1).pdf
potRelC3 <- terra::rast("./data/SYNMAPdata/SYNMAP_potentialRelativeFraction_c3.nc")
potRelC4 <- terra::rast("./data/SYNMAPdata/SYNMAP_potentialRelativeFraction_c4.nc")
potRelgrass  <- terra::rast("./data/SYNMAPdata/SYNMAP_potentialRelativeFraction_grasses.nc")

presRelC3 <- terra::rast("./data/SYNMAPdata/SYNMAP_presentRelativeFraction_c3.nc")
presRelC4 <- terra::rast("./data/SYNMAPdata/SYNMAP_presentRelativeFraction_c4.nc")
# I think below is the total fraction of cover w/in a point that is grass
presRelgrass  <- terra::rast("./data/SYNMAPdata/SYNMAP_presentRelativeFraction_grasses.nc")
# so, to get the total fraction of grass cover that is C3 and C4, we should multiply presRelC3 * presRelgrass (?) and presRelC4 * presRelgrass
## so, approximate c3 cover in each grid cell
c3_cover <- presRelC3 * presRelgrass
C4_cover <- presRelC4 * presRelgrass
## I think the above two are correct... so put into a stacked raster
cover <- c(c3_cover,  C4_cover)

# trim to the extent of CONUS
## first, get a shapefile of CONUS
CONUS <- st_read(dsn = "./data/CONUS_extent/", layer = "CONUS_boundary") %>% 
  st_make_valid()%>% 
  st_transform(st_crs(presRelgrass))# %>% 
  st_set_crs(crs(presRelgrass)) #%>% 
  #filter(ID == "main")

cover_mask <- cover %>% 
  mask(CONUS)
## randomly sample the points (?100,000 to start?)
cover_samp <- cover_mask %>% 
  spatSample(size = 5000, method = "random", na.rm = TRUE, replace = FALSE, as.points = TRUE) %>% 
  st_as_sf() %>% 
  rename(C3_cover = Band1, # rename columns 
         C4_cover = Band1.1) %>% 
  mutate(C3_cover = 100*C3_cover, # translate to % cover from proportion
         C4_cover = 100*C4_cover)

plot(cover_samp)

## save the cover d.f for further analysis
saveRDS(cover_samp, file = "./data/SYNMAPdata/sampledC3C4points.rds")
