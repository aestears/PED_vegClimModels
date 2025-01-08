#///////////////////
# Visualizing the spatial and temporal extent of the cover data
# Alice Stears
# 1/07/2025
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# load data  --------------------------------------------------------------

vegCovDat <- readRDS("./Data_processed/CoverData/DataForModels.RDS")


# make figures ------------------------------------------------------------
# shrub cover
(shrubYears <- vegCovDat %>% 
  dplyr::select(Source, Year, ShrubCover) %>%
  drop_na()  %>% 
ggplot() + 
  geom_density(aes(Year)) + 
  facet_wrap(~Source) + 
  ggtitle("Years when we have shrub cover data, by data source") + 
  theme_classic())

# forest cover
(forestYears <- vegCovDat %>% 
    dplyr::select(Source, Year, TotalTreeCover) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have tree cover data, by data source") + 
    theme_classic())

# CAM cover 
(camYears <- vegCovDat %>% 
    dplyr::select(Source, Year, CAMCover) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source, ncol = 1) + 
    ggtitle("Years when we have CAM cover data, by data source") + 
    theme_classic())

# herb + grass cover
(herbaceousYears <- vegCovDat %>% 
  dplyr::select(Source, Year, TotalHerbaceousCover) %>%
  drop_na()  %>% 
  ggplot() + 
  geom_density(aes(Year)) + 
  facet_wrap(~Source) + 
  ggtitle("Years when we have total herbaceous cover data, by data source") + 
    theme_classic())

# broad leaved tree cover
(broadLeavedTreeYears <- vegCovDat %>% 
    dplyr::select(Source, Year, BroadleavedTreeCover_prop) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Broad leaved Tree Cover proportion data, by data source") + 
    theme_classic())

# needle leaved tree cover
(needleLeavedTreeYears <- vegCovDat %>% 
    dplyr::select(Source, Year, NeedleLeavedTreeCover_prop) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Needle leaved Tree Cover proportion data, by data source") + 
    theme_classic())

# forb cover proportion
(forbYears <- vegCovDat %>% 
    dplyr::select(Source, Year, ForbCover_prop) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Forb Cover proportion data, by data source") + 
    theme_classic())


# c3 proportion
(c3Years <- vegCovDat %>% 
    dplyr::select(Source, Year, C3Cover_prop) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have C3 grass Cover proportion data, by data source") + 
    theme_classic())


# c3 proportion
(c4Years <- vegCovDat %>% 
    dplyr::select(Source, Year, C4Cover_prop) %>%
    drop_na()  %>% 
    ggplot() + 
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have C4 grass Cover proportion data, by data source") + 
    theme_classic())



(plots <- ggarrange(ggarrange(shrubYears, forestYears, camYears, herbaceousYears, ncol = 1),
                    ggarrange(
                    broadLeavedTreeYears, needleLeavedTreeYears, forbYears, c3Years, c4Years, ncol = 1)
)
)

bitmap(file = "./Figures/CoverDatFigures/TemporalExtentOfCoverData.bmp", 
       width = 15, height = 13, res = 200)
plots
dev.off()
# save images -------------------------------------------------------------


