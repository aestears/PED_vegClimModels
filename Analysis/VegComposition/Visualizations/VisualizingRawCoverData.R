#///////////////////
# Visualizing raw cover data from sources
# Alice Stears
# 11/11/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(ggpubr)
library(tidyterra)

# load data ---------------------------------------------------------------
# load shapefile version of "big composition dataset"
coverDat <- st_read("./Data_processed/DataForAnalysisPoints", layer = "vegCompPoints")
# coverDat <- readRdS("./Data_processed/CoverData/")
# treeCover_rast <- dat_all%>% 
#   #FIA_all %>% 
#   as.data.frame() %>%
#   filter(Source == "FIA") %>% 
#   terra::vect(geom = c("Lon", "Lat"), crs = st_crs("EPSG:4326")) 

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 8, fun = "mean") %>% 
  terra::project(crs(coverDat))


# For LANDFIRE data -------------------------------------------------------
# total tree cover data 
# rasterize data
treeCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))
(treeCov_plot_LF <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Tree % cover" ))

# conifer tree data -
# raw conifer cover
# rasterize data
conifCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifCover_plot_LF <- ggplot() + 
    geom_spatraster(data = conifCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Conifer % cover"))


# conifer Prop. of tree cover
# rasterize data
conifProportion_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifProportion_plot_LF <- ggplot() + 
    geom_spatraster(data = conifProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("LANDFIRE", subtitle = "Prop. of trees that are conifers"))

# broad-leaved tree data --
# raw deciduous cover
# rasterize data
decidCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidCover_plot_LF <- ggplot() + 
    geom_spatraster(data = decidCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Deciduous % cover"))

# deciduous Prop. of tree cover
# rasterize data
decidProportion_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidProportion_plot_LF <- ggplot() + 
    geom_spatraster(data = decidProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("LANDFIRE", subtitle = "Prop. of trees that are deciduous"))


# shrub data --
# rasterize data
shrubCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ShrbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(shrubCover_plot_LF <- ggplot() + 
    geom_spatraster(data = shrubCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Shrub % Cover"))

# graminoid data -
# rasterize data
totalGramCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_LF <- ggplot() + 
    geom_spatraster(data = totalGramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Total Herb. % Cover"))


# C3 grass data --
# raw C3 grass cover
# rasterize data
c3gramCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramCover_plot_LF <- ggplot() + 
    geom_spatraster(data = c3gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "C3 grass % Cover"))


# C3 grass Prop. of tree cover
# rasterize data
c3gramProportion_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramProportion_plot_LF <- ggplot() + 
    geom_spatraster(data = c3gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("LANDFIRE", subtitle = "Prop. of herb. cover that is C3"))


# C4 grass data --
# raw C4 grass cover
# rasterize data
C4gramCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramCover_plot_LF <- ggplot() + 
    geom_spatraster(data = C4gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "C4 grass % Cover"))

# C4 grass Prop. of tree cover
# rasterize data
C4gramProportion_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramProportion_plot_LF <- ggplot() + 
    geom_spatraster(data = C4gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("LANDFIRE", subtitle = "Prop. of herb. cover that is C4"))

# forb data 
# rasterize data
forbCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ForbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbCover_plot_LF <- ggplot() + 
    geom_spatraster(data = forbCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Forb % Cover"))

# forb proportion of herbaceous cover
# rasterize data
forbProportion_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "FrbCvr_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbProportion_plot_LF <- ggplot() + 
    geom_spatraster(data = forbProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("LANDFIRE", subtitle = "Prop. of herb. cover that is forbs"))

# gram + forb data -
# rasterize data
herbaceousCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(herbaceousCover_plot_LF <- ggplot() + 
    geom_spatraster(data = herbaceousCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Total Herb. % Cover"))


# CAM data -
# rasterize data
camCover_rast <- coverDat %>% 
  filter(Source == "LANDFIRE") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y =test_rast, # terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "CAMCovr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(camCover_plot_LF <- ggplot() + 
    geom_spatraster(data = camCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "CAM species % cover"))

# For FIA data ------------------------------------------------------------
# total tree cover data 
# rasterize data
treeCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))
(treeCov_plot_FIA <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Tree % cover"))

# conifer tree data -
# raw conifer cover
# rasterize data
conifCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = conifCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Conifer % cover"))


# conifer Prop. of tree cover
# rasterize data
conifProportion_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifProportion_plot_FIA <- ggplot() + 
    geom_spatraster(data = conifProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of trees that are conifers"))

# broad-leaved tree data --
# raw deciduous cover
# rasterize data
decidCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = decidCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Deciduous % cover"))

# deciduous Prop. of tree cover
# rasterize data
decidProportion_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidProportion_plot_FIA <- ggplot() + 
    geom_spatraster(data = decidProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of trees that are deciduous"))


# shrub data --
# rasterize data
shrubCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ShrbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(shrubCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = shrubCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Shrub % Cover"))

# graminoid data -
# rasterize data
totalGramCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = totalGramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Total Herb. % Cover"))


# C3 grass data --
# raw C3 grass cover
# rasterize data
c3gramCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = c3gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "C3 grass % Cover"))


# C3 grass Prop. of tree cover
# rasterize data
c3gramProportion_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramProportion_plot_FIA <- ggplot() + 
    geom_spatraster(data = c3gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of herb. cover that is C3"))


# C4 grass data --
# raw C4 grass cover
# rasterize data
C4gramCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = C4gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "C4 grass % Cover"))

# C4 grass Prop. of tree cover
# rasterize data
C4gramProportion_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramProportion_plot_FIA <- ggplot() + 
    geom_spatraster(data = C4gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of herb. cover that is C4"))

# forb data 
# rasterize data
forbCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ForbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = forbCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Forb % Cover"))

# forb proportion of herbaceous cover
# rasterize data
forbProportion_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "FrbCvr_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbProportion_plot_FIA <- ggplot() + 
    geom_spatraster(data = forbProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of herb. cover that is forbs"))

# gram + forb data -
# rasterize data
herbaceousCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(herbaceousCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = herbaceousCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Total Herb. % Cover"))


# CAM data -
# rasterize data
camCover_rast <- coverDat %>% 
  filter(Source == "FIA") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y =test_rast, # terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "CAMCovr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(camCover_plot_FIA <- ggplot() + 
    geom_spatraster(data = camCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "CAM species % cover"))

# For AIM/LDC Data --------------------------------------------------------
# total tree cover data 
# rasterize data
treeCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))
(treeCov_plot_AIM <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Tree % cover"))

# conifer tree data -
# raw conifer cover
# rasterize data
conifCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = conifCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Conifer % cover"))


# conifer Prop. of tree cover
# rasterize data
conifProportion_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifProportion_plot_AIM <- ggplot() + 
    geom_spatraster(data = conifProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of trees that are conifers"))

# broad-leaved tree data --
# raw deciduous cover
# rasterize data
decidCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = decidCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Deciduous % cover"))

# deciduous Prop. of tree cover
# rasterize data
decidProportion_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidProportion_plot_AIM <- ggplot() + 
    geom_spatraster(data = decidProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of trees that are deciduous"))


# shrub data --
# rasterize data
shrubCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ShrbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(shrubCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = shrubCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Shrub % Cover"))

# graminoid data -
# rasterize data
totalGramCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = totalGramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Total Herb. % Cover"))


# C3 grass data --
# raw C3 grass cover
# rasterize data
c3gramCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = c3gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "C3 grass % Cover"))


# C3 grass Prop. of tree cover
# rasterize data
c3gramProportion_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramProportion_plot_AIM <- ggplot() + 
    geom_spatraster(data = c3gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of herb. cover that is C3"))


# C4 grass data --
# raw C4 grass cover
# rasterize data
C4gramCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = C4gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "C4 grass % Cover"))

# C4 grass Prop. of tree cover
# rasterize data
C4gramProportion_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramProportion_plot_AIM <- ggplot() + 
    geom_spatraster(data = C4gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of herb. cover that is C4"))

# forb data 
# rasterize data
forbCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ForbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = forbCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Forb % Cover"))

# forb proportion of herbaceous cover
# rasterize data
forbProportion_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "FrbCvr_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbProportion_plot_AIM <- ggplot() + 
    geom_spatraster(data = forbProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of herb. cover that is forbs"))

# gram + forb data -
# rasterize data
herbaceousCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(herbaceousCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = herbaceousCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Total Herb. % Cover"))


# CAM data -
# rasterize data
camCover_rast <- coverDat %>% 
  filter(Source == "LDC") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y =test_rast, # terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "CAMCovr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(camCover_plot_AIM <- ggplot() + 
    geom_spatraster(data = camCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "CAM species % cover"))

# For RAP data ------------------------------------------------------------
# total tree cover data 
# rasterize data
treeCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))
(treeCov_plot_RAP <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Tree % cover"))

# conifer tree data -
# raw conifer cover
# rasterize data
conifCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = conifCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Conifer % cover"))


# conifer Prop. of tree cover
# rasterize data
conifProportion_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CnfTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(conifProportion_plot_RAP <- ggplot() + 
    geom_spatraster(data = conifProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of trees that are conifers"))

# broad-leaved tree data --
# raw deciduous cover
# rasterize data
decidCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = decidCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Deciduous % cover"))

# deciduous Prop. of tree cover
# rasterize data
decidProportion_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "AngTrC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(decidProportion_plot_RAP <- ggplot() + 
    geom_spatraster(data = decidProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of trees that are deciduous"))


# shrub data --
# rasterize data
shrubCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ShrbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(shrubCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = shrubCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Shrub % Cover"))

# graminoid data -
# rasterize data
totalGramCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = totalGramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Total Herb. % Cover"))


# C3 grass data --
# raw C3 grass cover
# rasterize data
c3gramCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = c3gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "C3 grass % Cover"))


# C3 grass Prop. of tree cover
# rasterize data
c3gramProportion_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C3GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(c3gramProportion_plot_RAP <- ggplot() + 
    geom_spatraster(data = c3gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of herb. cover that is C3"))


# C4 grass data --
# raw C4 grass cover
# rasterize data
C4gramCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = C4gramCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "C4 grass % Cover"))

# C4 grass Prop. of tree cover
# rasterize data
C4gramProportion_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "C4GrmC_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(C4gramProportion_plot_RAP <- ggplot() + 
    geom_spatraster(data = C4gramProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of herb. cover that is C4"))

# forb data 
# rasterize data
forbCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ForbCvr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = forbCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Forb % Cover"))

# forb proportion of herbaceous cover
# rasterize data
forbProportion_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, #terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "FrbCvr_", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(forbProportion_plot_RAP <- ggplot() + 
    geom_spatraster(data = forbProportion_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop. of herb. cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of herb. cover that is forbs"))

# gram + forb data -
# rasterize data
herbaceousCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlHrbC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(herbaceousCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = herbaceousCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Total Herb. % Cover"))


# CAM data -
# rasterize data
camCover_rast <- coverDat %>% 
  filter(Source == "RAP") %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y =test_rast, # terra::aggregate(test_rast, fact = 2, fun = "mean"), 
                   field = "CAMCovr", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(camCover_plot_RAP <- ggplot() + 
    geom_spatraster(data = camCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "CAM species % cover"))


# For data averaged across each grid cell ---------------------------------
# load shapefile version of "big composition dataset"
coverDatAvg <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds") %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = ("PROJCRS[\"unnamed\",BASEGEOGCRS[\"unknown\", DATUM[\"unknown\",
   ELLIPSOID[\"Spheroid\",6378137,298.257223563, LENGTHUNIT[\"metre\",1,  ID[\"EPSG\",9001]]]],   PRIMEM[\"Greenwich\",0,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433,  ID[\"EPSG\",9122]]]],
                                             \nCONVERSION[\"Lambert Conic Conformal (2SP)\",   
                                             METHOD[\"Lambert Conic Conformal (2SP)\",   ID[\"EPSG\",9802]],   
                                             PARAMETER[\"Latitude of false origin\",42.5,   ANGLEUNIT[\"degree\",0.0174532925199433],   
                                             ID[\"EPSG\",8821]],   PARAMETER[\"Longitude of false origin\",-100,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   ID[\"EPSG\",8822]],  
                                             PARAMETER[\"Latitude of 1st standard parallel\",25,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   
                                             ID[\"EPSG\",8823]],   PARAMETER[\"Latitude of 2nd standard parallel\",60,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   ID[\"EPSG\",8824]],  
                                             PARAMETER[\"Easting at false origin\",0,   LENGTHUNIT[\"metre\",1], 
                                             ID[\"EPSG\",8826]],   PARAMETER[\"Northing at false origin\",0,   
                                             LENGTHUNIT[\"metre\",1],   ID[\"EPSG\",8827]]],\nCS[Cartesian,2],  
                                             AXIS[\"easting\",east,   ORDER[1],   LENGTHUNIT[\"metre\",1, 
                                             ID[\"EPSG\",9001]]],   AXIS[\"northing\",north,   ORDER[2],   
                                             LENGTHUNIT[\"metre\",1,  ID[\"EPSG\",9001]]]]")) %>% 
  st_transform(crs(test_rast))

# use 'test_rast' from above

# total tree cover data 
# rasterize data
treeCover_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TotalTreeCover", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(treeCov_plot_AVG <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("Averaged Data", subtitle = "Tree % cover" ))

# shrub cover data 
# rasterize data
shrubCover_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ShrubCover", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(shrubCov_plot_AVG <- ggplot() + 
    geom_spatraster(data = shrubCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("Averaged Data", subtitle = "Shrub % cover" ))

# total herbaceous cover data 
# rasterize data
herbCover_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TotalHerbaceousCover", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(herbCover_plot_AVG <- ggplot() + 
    geom_spatraster(data = herbCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("Averaged Data", subtitle = "Total Herb. % cover" ))

# total CAM cover data 
# rasterize data
camCover_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "CAMCover", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(camCover_plot_AVG <- ggplot() + 
    geom_spatraster(data = camCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("Averaged Data", subtitle = "CAM species % cover" ))


# conifer cover proportion data 
# rasterize data
coniferCoverProp_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "NeedleLeavedTreeCover_prop", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(coniferCoverProp_plot_AVG <- ggplot() + 
    geom_spatraster(data = coniferCoverProp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop."), 
                         limits = c(0,1)) +
    ggtitle("Averaged Data", subtitle = "Prop. of trees that are conifers" ))


# conifer cover proportion data 
# rasterize data
decidCoverProp_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "BroadLeavedTreeCover_prop", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(decidCoverProp_plot_AVG <- ggplot() + 
    geom_spatraster(data = decidCoverProp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop."), 
                         limits = c(0,1)) +
    ggtitle("Averaged Data", subtitle = "Prop. of trees that are deciduous" ))

# forb cover proportion data 
# rasterize data
forbCoverProp_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "ForbCover_prop" , 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(forbCoverProp_prop_AVG <- ggplot() + 
    geom_spatraster(data = forbCoverProp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop."), 
                         limits = c(0,1)) +
    ggtitle("Averaged Data", subtitle = "Prop. of herb. that are forbs" )) 

# C3 cover proportion data 
# rasterize data
C3CoverProp_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "C3GramCover_prop", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(C3CoverProp_prop_AVG <- ggplot() + 
    geom_spatraster(data = C3CoverProp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop."), 
                         limits = c(0,1)) +
    ggtitle("Averaged Data", subtitle = "Prop. of herb. that are C3 grasses" )) 


# C4 cover proportion data 
# rasterize data
C4CoverProp_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "C4GramCover_prop", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(C4CoverProp_prop_AVG <- ggplot() + 
    geom_spatraster(data = C4CoverProp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "B", guide = guide_colorbar(title = "Prop."), 
                         limits = c(0,1)) +
    ggtitle("Averaged Data", subtitle = "Prop. of herb. that are C4 grasses" ))                                     


# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/coverDatAvgFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
ggarrange(
  ggarrange(treeCov_plot_AIM, treeCov_plot_FIA, treeCov_plot_LF, treeCov_plot_RAP, treeCov_plot_AVG, nrow = 1, common.legend = TRUE),
  ggarrange(conifCover_plot_AIM, conifCover_plot_FIA, conifCover_plot_LF, conifCover_plot_RAP, NA, nrow = 1, common.legend = TRUE),
  ggarrange(conifProportion_plot_AIM, conifProportion_plot_FIA, conifProportion_plot_LF, conifProportion_plot_RAP,coniferCoverProp_plot_AVG, nrow = 1, common.legend = TRUE),
  ggarrange(decidCover_plot_AIM, decidCover_plot_FIA, decidCover_plot_LF, decidCover_plot_RAP,NA, nrow = 1, common.legend = TRUE),
  ggarrange(decidProportion_plot_AIM, decidProportion_plot_FIA, decidProportion_plot_LF, decidProportion_plot_RAP, decidCoverProp_plot_AVG,nrow = 1, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
ggexport(
  filename = "./Figures/coverDatFigures/TreeFigures.pdf", 
  width = 14, height = 17)

# grass figures
ggarrange(
  ggarrange(totalGramCover_plot_AIM, totalGramCover_plot_FIA, totalGramCover_plot_LF, totalGramCover_plot_RAP, herbCover_plot_AVG, nrow = 1, common.legend = TRUE),
  ggarrange(c3gramCover_plot_AIM, c3gramCover_plot_FIA, c3gramCover_plot_LF, c3gramCover_plot_RAP, NA, nrow = 1, common.legend = TRUE),
  ggarrange(c3gramProportion_plot_AIM, c3gramProportion_plot_FIA, c3gramProportion_plot_LF, c3gramProportion_plot_RAP, C3CoverProp_prop_AVG, nrow = 1, common.legend = TRUE),
  ggarrange(C4gramCover_plot_AIM, C4gramCover_plot_FIA, C4gramCover_plot_LF, C4gramCover_plot_RAP, NA, nrow = 1, common.legend = TRUE),
  ggarrange(C4gramProportion_plot_AIM, C4gramProportion_plot_FIA, C4gramProportion_plot_LF, C4gramProportion_plot_RAP, C4CoverProp_prop_AVG, nrow = 1, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
ggexport(
  filename = "./Figures/coverDatFigures/GrassFigures.pdf",
  width = 14, height = 17)

# forbs, herbaceous, Shrub, and CAM figures
ggarrange(
  ggarrange(shrubCover_plot_AIM, shrubCover_plot_FIA, shrubCover_plot_LF, shrubCover_plot_RAP, shrubCov_plot_AVG,  nrow = 1, common.legend = TRUE),
  ggarrange(herbaceousCover_plot_AIM, herbaceousCover_plot_FIA, herbaceousCover_plot_LF, herbaceousCover_plot_RAP, herbCover_plot_AVG, nrow = 1, common.legend = TRUE),
  ggarrange(forbCover_plot_AIM, forbCover_plot_FIA, forbCover_plot_LF, forbCover_plot_RAP , NA, nrow = 1, common.legend = TRUE),
  ggarrange(forbProportion_plot_AIM, forbProportion_plot_FIA, forbProportion_plot_LF, forbProportion_plot_RAP, forbCoverProp_prop_AVG,  nrow = 1, common.legend = TRUE),
  ggarrange(camCover_plot_AIM, camCover_plot_FIA, camCover_plot_LF, camCover_plot_RAP,nrow = 1, camCover_plot_AVG, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
  ggexport(
    filename = "./Figures/coverDatFigures/ForbHerbaceousCamShrubFigures.pdf",
    width = 14, height = 17)


# Testing ---------------------------------
# load shapefile version of "big composition dataset"
coverDatAvg <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds") %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = ("PROJCRS[\"unnamed\",BASEGEOGCRS[\"unknown\", DATUM[\"unknown\",
   ELLIPSOID[\"Spheroid\",6378137,298.257223563, LENGTHUNIT[\"metre\",1,  ID[\"EPSG\",9001]]]],   PRIMEM[\"Greenwich\",0,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433,  ID[\"EPSG\",9122]]]],
                                             \nCONVERSION[\"Lambert Conic Conformal (2SP)\",   
                                             METHOD[\"Lambert Conic Conformal (2SP)\",   ID[\"EPSG\",9802]],   
                                             PARAMETER[\"Latitude of false origin\",42.5,   ANGLEUNIT[\"degree\",0.0174532925199433],   
                                             ID[\"EPSG\",8821]],   PARAMETER[\"Longitude of false origin\",-100,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   ID[\"EPSG\",8822]],  
                                             PARAMETER[\"Latitude of 1st standard parallel\",25,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   
                                             ID[\"EPSG\",8823]],   PARAMETER[\"Latitude of 2nd standard parallel\",60,   
                                             ANGLEUNIT[\"degree\",0.0174532925199433],   ID[\"EPSG\",8824]],  
                                             PARAMETER[\"Easting at false origin\",0,   LENGTHUNIT[\"metre\",1], 
                                             ID[\"EPSG\",8826]],   PARAMETER[\"Northing at false origin\",0,   
                                             LENGTHUNIT[\"metre\",1],   ID[\"EPSG\",8827]]],\nCS[Cartesian,2],  
                                             AXIS[\"easting\",east,   ORDER[1],   LENGTHUNIT[\"metre\",1, 
                                             ID[\"EPSG\",9001]]],   AXIS[\"northing\",north,   ORDER[2],   
                                             LENGTHUNIT[\"metre\",1,  ID[\"EPSG\",9001]]]]")) %>% 
  st_transform(crs(test_rast))

# use 'test_rast' from above

# total tree cover data 
# rasterize data
treeCover_rast <- coverDatAvg %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TotalTreeCover", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-128, -65, 23, 51))
(treeCov_plot_AVG <- ggplot() + 
    geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("Averaged Data", subtitle = "Tree % cover" ))
