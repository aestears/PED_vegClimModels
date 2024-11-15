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

# load data ---------------------------------------------------------------
# load shapefile version of "big composition dataset"
coverDat <- st_read("./Data_processed/DataForAnalysisPoints", layer = "vegCompPoints")

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 16, fun = "mean") %>% 
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
(treeCov_plot_LF <- ggplot(treeCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifCover_plot_LF <- ggplot(conifCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifProportion_plot_LF <- ggplot(conifProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
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
(decidCover_plot_LF <- ggplot(decidCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(decidProportion_plot_LF <- ggplot(decidProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
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
(shrubCover_plot_LF <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
                   field = "TtlGrmC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_LF <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Grass % Cover"))


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
(c3gramCover_plot_LF <- ggplot(c3gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(c3gramProportion_plot_LF <- ggplot(c3gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(C4gramCover_plot_LF <- ggplot(C4gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(C4gramProportion_plot_LF <- ggplot(C4gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(forbCover_plot_LF <- ggplot(forbCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(forbProportion_plot_LF <- ggplot(forbProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(herbaceousCover_plot_LF <- ggplot(herbaceousCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("LANDFIRE", subtitle = "Total Herbaceous % Cover"))


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
(camCover_plot_LF <- ggplot(camCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(treeCov_plot_FIA <- ggplot(treeCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifCover_plot_FIA <- ggplot(conifCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifProportion_plot_FIA <- ggplot(conifProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
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
(decidCover_plot_FIA <- ggplot(decidCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(decidProportion_plot_FIA <- ggplot(decidProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("FIA", subtitle = "Prop. of trees that are conifers"))


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
(shrubCover_plot_FIA <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
                   field = "TtlGrmC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_FIA <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Grass % Cover"))


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
(c3gramCover_plot_FIA <- ggplot(c3gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(c3gramProportion_plot_FIA <- ggplot(c3gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(C4gramCover_plot_FIA <- ggplot(C4gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(C4gramProportion_plot_FIA <- ggplot(C4gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(forbCover_plot_FIA <- ggplot(forbCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(forbProportion_plot_FIA <- ggplot(forbProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(herbaceousCover_plot_FIA <- ggplot(herbaceousCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("FIA", subtitle = "Total Herbaceous % Cover"))


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
(camCover_plot_FIA <- ggplot(camCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(treeCov_plot_AIM <- ggplot(treeCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifCover_plot_AIM <- ggplot(conifCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifProportion_plot_AIM <- ggplot(conifProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
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
(decidCover_plot_AIM <- ggplot(decidCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(decidProportion_plot_AIM <- ggplot(decidProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("AIM", subtitle = "Prop. of trees that are conifers"))


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
(shrubCover_plot_AIM <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
                   field = "TtlGrmC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_AIM <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Grass % Cover"))


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
(c3gramCover_plot_AIM <- ggplot(c3gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(c3gramProportion_plot_AIM <- ggplot(c3gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(C4gramCover_plot_AIM <- ggplot(C4gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(C4gramProportion_plot_AIM <- ggplot(C4gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(forbCover_plot_AIM <- ggplot(forbCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(forbProportion_plot_AIM <- ggplot(forbProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(herbaceousCover_plot_AIM <- ggplot(herbaceousCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("AIM", subtitle = "Total Herbaceous % Cover"))


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
(camCover_plot_AIM <- ggplot(camCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(treeCov_plot_RAP <- ggplot(treeCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifCover_plot_RAP <- ggplot(conifCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(conifProportion_plot_RAP <- ggplot(conifProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
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
(decidCover_plot_RAP <- ggplot(decidCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(decidProportion_plot_RAP <- ggplot(decidProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of tree cover"), 
                         limits = c(0,1)) +
    ggtitle("RAP", subtitle = "Prop. of trees that are conifers"))


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
(shrubCover_plot_RAP <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
                   field = "TtlGrmC", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
(totalGramCover_plot_RAP <- ggplot(shrubCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Grass % Cover"))


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
(c3gramCover_plot_RAP <- ggplot(c3gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(c3gramProportion_plot_RAP <- ggplot(c3gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(C4gramCover_plot_RAP <- ggplot(C4gramCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(C4gramProportion_plot_RAP <- ggplot(C4gramProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(forbCover_plot_RAP <- ggplot(forbCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
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
(forbProportion_plot_RAP <- ggplot(forbProportion_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "Prop. of herb. cover"), 
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
(herbaceousCover_plot_RAP <- ggplot(herbaceousCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "Total Herbaceous % Cover"))


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
(camCover_plot_RAP <- ggplot(camCover_rast) + 
    geom_raster(aes(x, y, fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    xlim(c(-130, -65)) + 
    ylim(c(25, 50))+
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(0,100)) +
    ggtitle("RAP", subtitle = "CAM species % cover"))


# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/CoverDatFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
ggarrange(
  ggarrange(treeCov_plot_AIM, treeCov_plot_FIA, treeCov_plot_LF, treeCov_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(conifCover_plot_AIM, conifCover_plot_FIA, conifCover_plot_LF, conifCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(conifProportion_plot_AIM, conifProportion_plot_FIA, conifProportion_plot_LF, conifProportion_plot_RAP,nrow = 1, common.legend = TRUE),
  ggarrange(decidCover_plot_AIM, decidCover_plot_FIA, decidCover_plot_LF, decidCover_plot_RAP,nrow = 1, common.legend = TRUE),
  ggarrange(decidProportion_plot_AIM, decidProportion_plot_FIA, decidProportion_plot_LF, decidProportion_plot_RAP, nrow = 1, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
ggexport(
  filename = "./Figures/CoverDatFigures/TreeFigures.pdf", 
  width = 11, height = 16)

# grass figures
ggarrange(
  ggarrange(totalGramCover_plot_AIM, totalGramCover_plot_FIA, totalGramCover_plot_LF, totalGramCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(c3gramCover_plot_AIM, c3gramCover_plot_FIA, c3gramCover_plot_LF, c3gramCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(c3gramProportion_plot_AIM, c3gramProportion_plot_FIA, c3gramProportion_plot_LF, c3gramProportion_plot_RAP,nrow = 1, common.legend = TRUE),
  ggarrange(C4gramCover_plot_AIM, C4gramCover_plot_FIA, C4gramCover_plot_LF, C4gramCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(C4gramProportion_plot_AIM, C4gramProportion_plot_FIA, C4gramProportion_plot_LF, C4gramProportion_plot_RAP,nrow = 1, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
ggexport(
  filename = "./Figures/CoverDatFigures/GrassFigures.pdf",
  width = 11, height = 16)

# forbs, herbaceous, Shrub, and CAM figures
ggarrange(
  ggarrange(shrubCover_plot_AIM, shrubCover_plot_FIA, shrubCover_plot_LF, shrubCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(herbaceousCover_plot_AIM, herbaceousCover_plot_FIA, herbaceousCover_plot_LF, herbaceousCover_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(forbCover_plot_AIM, forbCover_plot_FIA, forbCover_plot_LF, forbCover_plot_RAP ,nrow = 1, common.legend = TRUE),
  ggarrange(forbProportion_plot_AIM, forbProportion_plot_FIA, forbProportion_plot_LF, forbProportion_plot_RAP, nrow = 1, common.legend = TRUE),
  ggarrange(camCover_plot_AIM, camCover_plot_FIA, camCover_plot_LF, camCover_plot_RAP,nrow = 1, common.legend = TRUE),
  nrow = 5, ncol = 1) %>% 
  ggexport(
    filename = "./Figures/CoverDatFigures/ForbHerbaceousCamShrubFigures.pdf",
    width = 11, height = 16)
