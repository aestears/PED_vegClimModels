## Code from Martin Holdrege, April 23, 2026
# calculated the total non-forested area (based on RAP)

# this is for help calibrating the cover models (i.e. to 
# correct the predictions of non-forested area)

#source('Functions/init.R')

library(terra)
library(here)

mask_name <- "mask-lcmap90-fire90" # "mask-lcmap50-fire50" 


# fraction of each daymet pixel that is not forest, where not forest
# means that at the 30m scale there is <1% tree cover

here::i_am("Analysis/VegComposition/DataPrep/09_non-forest_area.R")
path_notForest <- c("Data_processed/RAP_v3_fracNotForest_mask-lcmap90-fire90_2019-2023_1000m.tif") #file.path(paths$large, "Data_processed", "CoverData", "rap", paste0("RAP_v3_fracNotForest_", mask_name, "_2019-2023_1000m.tif"))

notForest <- rast(here::here(path_notForest))

# area of each pixel in m² (accounts for projection distortion)
pixel_area <- cellSize(notForest, unit = "m")

# total non-forest area (m² → km²)
area_not_forest_m2 <- global(notForest * pixel_area, fun = "sum", na.rm = TRUE)
area_not_forest_km2 <- area_not_forest_m2$sum / 1e6
#cat("Total non-forest area:", round(area_not_forest_km2), "km^2", '(', mask_name, ")\n")
# Total non-forest area: 1111716 km^2 ( mask-lcmap90-fire90 )
# Total non-forest area: 1370498 km^2 ( mask-lcmap50-fire50 )

# mean proportion non-forest (area-weighted)
mean_frac <- global(notForest * pixel_area, fun = "sum", na.rm = TRUE)$sum /
  global(pixel_area * !is.na(notForest), fun = "sum", na.rm = TRUE)$sum
#cat("Mean proportion non-forest:", round(mean_frac, 3), '(', mask_name, ")\n")
# Mean proportion non-forest: 0.313 # for mask-lcmap90-fire90
# nMean proportion non-forest: 0.278 ( mask-lcmap50-fire50 )
