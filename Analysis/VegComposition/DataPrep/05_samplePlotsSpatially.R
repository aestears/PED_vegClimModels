#///////////////////
# Averaging values from plots w/in each dayMet gridcell
# Alice Stears
# 8/19/24
#///////////////////


# clear environment -------------------------------------------------------

rm(list=ls()) 

# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(stars)
library(lwgeom)
library(tidyterra)
library(ggpubr)
library(parallel)

# Load data ---------------------------------------------------------------

# get veg data
dat_temp <- readRDS("./Data_processed/CoverData/DataForModels.RDS") 
# dayMet extent 
testTemp <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(dat_temp))

layerNames <- c("ShrubCover", "TotalHerbaceousCover",  
  "TotalTreeCover","C3GramCover_prop", "C4GramCover_prop", "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop", "BareGroundCover"
  )

# tidy up data frame ------------------------------------------------
# # make veg data projection the same as raster data
# v <- vect(st_drop_geometry(dat_temp)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(dat_temp))
# y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
dat <- dat_temp %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
 # sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(Year)) %>% 
  mutate(rowID = seq(1:nrow(.))) %>% 
  select(Year, Year_char, UniquID:Day, ShrbCvr:TtlHrbC, BrGrndC, LttrCvr, LttrDpt,TrBsA_2, AngTrC_:Lat, Source, geometry, rowID)

# make sure the raster data is in the appropriate projection
# test <- test %>%
#   terra::project(crs(y))
st_crs(dat) == st_crs(test)

## rename the columns in 'dat' 
# fix names
dat  <- dat %>%
  rename(UniqueID = UniquID,
         StateUnitCounty = SttUntC,
         PlotCondition = PltCndt,
         ShrubCover = ShrbCvr,
         TotalHerbaceousCover = TtlHrbC,
         TotalTreeCover = TtlTrCv,
         TotalGramCover = TtlGrmC,
         AnnualHerbGramCover = AnnlHGC,
         PerennialHerbGramCover = PrnnHGC,
         C3GramCover = C3GrmCv,
         C3GramCover_prop = C3GrmC_,
         C4GramCover = C4GrmCv,
         C4GramCover_prop = C4GrmC_,
         ForbCover = ForbCvr,
         ForbCover_prop = FrbCvr_,
         AngioTreeCover = AngTrCv,
         AngioTreeCover_prop = AngTrC_,
         ConifTreeCover = CnfTrCv,
         ConifTreeCover_prop = CnfTrC_,
         TreeBasalArea = TrBsA_2,
         BareGroundCover = BrGrndC,
         LitterCover = LttrCvr,
         LitterDepth = LttrDpt)

dat <- ungroup(dat)

dat2 <- dat %>% 
  filter(Year >= 2000) #%>% 

# # OPTION to filter out some of the LANDFIRE data  ------------------------------
dat2_noLF <- dat2 %>%
  filter(Source != "LANDFIRE")

dat2_LFonly <- dat2 %>%
  filter(Source == "LANDFIRE")

## aggregate the dayMet grid up by 3
test_big <- test %>%
  terra::aggregate(fact = 3, fun = "mean") 

# area of these cells (in m)
#plot(test_big$daymet_v4_prcp_monttl_na_1980_1)
# terra::cellSize(test)
# terra::cellSize(test_big)

## add the aggregated dayMet ID to each LF observation
dat2_LFonly_cellID <- dat2_LFonly %>%
  cbind(terra::qdf45tvegDat_newTest_withOGdata(x = test_big$daymet_v4_prcp_monttl_na_1980_1, y = dat2_LFonly, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])
dat2_LFonly_cellID$uniqueRowID <- 1:nrow(dat2_LFonly_cellID)
# combine year and cellID info into one column
dat2_LFonly_cellID$cellID_year <- paste0(dat2_LFonly_cellID$cell, "_", dat2_LFonly_cellID$Year)

## if, for a given data type in a given year, there are > 1 observation in an aggregated grid cell, average the values together
testTest <- lapply(X = layerNames, FUN = function(x) {
  ## get data just for the focal cover type
  temp <- dat2_LFonly_cellID[,c("Year", "Lat", "Lon", x, "cell", "uniqueRowID", "cellID_year")] %>%
    st_drop_geometry() %>%
    drop_na()
  if (nrow(temp) > 0) {
    ## get counts of the number of observations within an aggregated cell in the same year
    temp2 <- temp %>%
      group_by(Year, cell, cellID_year) %>%
      summarise(cell_n = n())
    # get names of those cells/years that only have one observations
    singleCellYears <- temp2[temp2$cell_n==1, c("cellID_year")]
    # get names of those cells/years that have more than one observation
    dupCellsYears <- temp2[temp2$cell_n!=1, c("cellID_year")]
    
    ## keep all of the cells that have only 1 observation in a given year
    out_singleObs <- temp %>%
      filter(cellID_year %in% singleCellYears$cellID_year)
    
    ## for those cells that have >1 observation in a given year, average across those observations
    # names of duplicated rows
    dupCellKeepIDs <- apply(dupCellsYears, MARGIN = 1, FUN = function(y) {
      temp3 <- temp %>%
        filter(cellID_year == y["cellID_year"]
        ) %>%
        summarize(Year = mean(Year), 
                  Lat = mean(Lat), 
                  Lon = mean(Lon), 
                  x = mean(.data[[x]]), 
                  cell = mean(cell), 
                  uniqueRowID = NA, 
                  cellID_year = unique(cellID_year)) 
      names(temp3)[4] <- x
      #slice_sample(n = 1) %>%
      #select(uniqueRowID)
      return(temp3)
    }
    )
    out_multObs <- dupCellKeepIDs %>% 
      purrr::list_rbind()
    # temp %>%
    # filter(uniqueRowID %in% dupCellKeepIDs)
    
    ## put together the single observation cells and the duplicate observation cells and save
    out <- out_singleObs %>%
      rbind(out_multObs)
  }  
  
})

# get data that will be joined to the filtered data 
tempTemp <- dat2_LFonly_cellID %>% select(Year, cell, cellID_year) %>% 
  st_drop_geometry() %>% unique() %>% group_by(Year, cell, cellID_year) 

dat2_LFonly_filtered <- testTest[[1]] %>%
  full_join(testTest[[2]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[3]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[4]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[5]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[6]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[7]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[8]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  left_join(tempTemp, by = c("Year", "cell", "cellID_year"
  )) %>%
  mutate(BareGroundCover = NA, 
         Year_char = as.character(Year), 
         UniqueID = NA, 
         StateUnitCounty = NA, 
         Plot = NA, 
         PlotCondition = NA, 
         Month = NA, 
         Day = NA,
         burnedMoreThan20YearsAgo = NA,
         group = NA, 
         NA_L2CODE = NA,
         NA_L2NAME = NA,
         NA_L1CODE = NA,              
         NA_L1NAME = NA,
         NA_L2KEY = NA,
         NA_L1KEY = NA,
         Shape_Leng = NA,
         Shape_Area = NA, 
         Source = "LANDFIRE") %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), remove = FALSE, crs = sf::st_crs(dat2)) %>% 
  mutate(rowID = NA) %>% 
  select(names(dat2_noLF %>% select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
                                       AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth))), 'cell') 

# add the sampled LF data onto the un-sampled LF data
dat2 <- dat2_noLF %>%
  select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
            AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth)) %>%
  rbind(dat2_LFonly_filtered %>% select(-cell))


# Separate tree info into 'complete' and 'all' ---------------------
# complete tree observations have information for total tree cover, broad leaved tree cover, and needle-leaved tree cover
# all tree observations include complete tree information as well as observations that have information for total tree cover only

dat2 <- dat2 %>% 
  #slice(1:1000) %>% 
  mutate(TotalTreeCover_completeObsOnly = TotalTreeCover)   # make new column for complete obs only
# replace values of this new variable for locations that don't have observations for NL trees or BL trees
dat2[is.na(dat2$AngioTreeCover_prop) & is.na(dat2$ConifTreeCover_prop),]$TotalTreeCover_completeObsOnly <- NA



# For RAP, exclude all cover observations prior to 2010 (when we h --------
dat2[dat2$Source == "RAP" & dat2$Year < 2010,] <- NA
dat2 <- dat2 %>% 
  filter(!is.na(Year))


# save data pre-spatial averaging
saveRDS(dat2, "./Data_processed/CoverData/data_beforeSpatialAveraging_sampledLANDFIRE.rds")
#dat2 <- readRDS("./Data_processed/CoverData/data_beforeSpatialAveraging_sampledLANDFIRE.rds")
# Now do spatial averaging -- Get the 'name' of the daymet cell that each observation lies within --------
crs(dat2) == crs(test)
dat2_cellID <- dat2 %>% 
  cbind(terra::extract(x = test$daymet_v4_prcp_monttl_na_1980_1, y = dat2, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])

#set up parallel processing for this part of the workflow
detectCores() # there are 16 cores
## get unique years in the dat2_cellID dataset
uniqueYears <- sort(unique(dat2_cellID$Year))
# run parallel process

dat2_avgsBySource_parallel <- #future_lapply
  mclapply(X = 1:length(uniqueYears), #MARGIN = 1,
           FUN = function(X) {
             temp <- dat2_cellID %>% 
               st_drop_geometry() %>% 
               dplyr::filter(Year %in% uniqueYears[X]) %>% 
               group_by(Year, cell, x, y 
                        ,Source
               ) %>% 
               #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
               summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
                         ShrubCover_sd = sd(ShrubCover, na.rm = TRUE),
                         ShrubCover = mean(ShrubCover, na.rm = TRUE),
                         TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
                         TotalHerbaceousCover_sd = sd(TotalHerbaceousCover, na.rm = TRUE),
                         TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE),
                         #TotalHerbaceousCover_n2 = n(),
                         TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
                         TotalTreeCover_sd = sd(TotalTreeCover, na.rm = TRUE),
                         TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE),
                         TotalTreeCover_compObs_n = sum(!is.na(TotalTreeCover_completeObsOnly)),
                         TotalTreeCover_compObs_sd = sd(TotalTreeCover_completeObsOnly, na.rm = TRUE),
                         TotalTreeCover_compObs = mean(TotalTreeCover_completeObsOnly, na.rm = TRUE),
                         C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
                         C3GramCover_prop_sd = sd(C3GramCover_prop, na.rm = TRUE),
                         C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE),
                         C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
                         C4GramCover_prop_sd = sd(C4GramCover_prop, na.rm = TRUE),
                         C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE),
                         ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
                         ForbCover_prop_sd = sd(ForbCover_prop, na.rm = TRUE),
                         ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE),
                         AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
                         AngioTreeCover_prop_sd = sd(AngioTreeCover_prop, na.rm = TRUE),
                         AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE),
                         ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
                         ConifTreeCover_prop_sd = sd(ConifTreeCover_prop, na.rm = TRUE),
                         ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE),
                         BareGroundCover_n = sum(!is.na(BareGroundCover)),
                         BareGroundCover_sd = sd(BareGroundCover, na.rm = TRUE),
                         BareGroundCover = mean(BareGroundCover, na.rm = TRUE)
               )
             return(temp)
           }
  )

dat2_avgsBySource <- dat2_avgsBySource_parallel %>% 
  purrr::list_rbind()

## get averages across all data sources
#set up parallel processing for this part of the workflow
#detectCores() # there are 16 cores
## get unique years in the dat2_cellID dataset
uniqueYears <-sort(unique(dat2_cellID$Year))
# run parallel process

dat2_avgs_parallel <- #future_lapply
  mclapply(X = 1:length(uniqueYears), #MARGIN = 1,
           FUN = function(X) {
             temp <- dat2_cellID %>% 
               st_drop_geometry() %>% 
               dplyr::filter(Year %in% uniqueYears[X]) %>% 
               group_by(Year, cell, x, y 
                        #,Source
               ) %>% 
               #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
               summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
                         ShrubCover_sd = sd(ShrubCover, na.rm = TRUE),
                         ShrubCover = mean(ShrubCover, na.rm = TRUE),
                         TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
                         TotalHerbaceousCover_sd = sd(TotalHerbaceousCover, na.rm = TRUE),
                         TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE),
                         #TotalHerbaceousCover_n2 = n(),
                         TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
                         TotalTreeCover_sd = sd(TotalTreeCover, na.rm = TRUE),
                         TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE),
                         TotalTreeCover_compObs_n = sum(!is.na(TotalTreeCover_completeObsOnly)),
                         TotalTreeCover_compObs_sd = sd(TotalTreeCover_completeObsOnly, na.rm = TRUE),
                         TotalTreeCover_compObs = mean(TotalTreeCover_completeObsOnly, na.rm = TRUE),
                         C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
                         C3GramCover_prop_sd = sd(C3GramCover_prop, na.rm = TRUE),
                         C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE),
                         C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
                         C4GramCover_prop_sd = sd(C4GramCover_prop, na.rm = TRUE),
                         C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE),
                         ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
                         ForbCover_prop_sd = sd(ForbCover_prop, na.rm = TRUE),
                         ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE),
                         AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
                         AngioTreeCover_prop_sd = sd(AngioTreeCover_prop, na.rm = TRUE),
                         AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE),
                         ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
                         ConifTreeCover_prop_sd = sd(ConifTreeCover_prop, na.rm = TRUE),
                         ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE),
                         BareGroundCover_n = sum(!is.na(BareGroundCover)),
                         BareGroundCover_sd = sd(BareGroundCover, na.rm = TRUE),
                         BareGroundCover = mean(BareGroundCover, na.rm = TRUE)
               )
             return(temp)
           }
  )


dat2_avgs <- dat2_avgs_parallel %>% 
  purrr::list_rbind()

# save data ---------------------------------------------------------------
# spatially averaged data (across all data sources)
saveRDS(dat2_avgs, "./Data_processed/CoverData/spatiallyAverageData_intermediate_test5_sampledLANDFIRE.rds")
#dat2_avgs <- readRDS("./Data_processed/CoverData/spatiallyAverageData_intermediate_test5_sampledLANDFIRE.rds")
# spatially averaged data (For Each data source)
saveRDS(dat2_avgsBySource, "./Data_processed/CoverData/spatiallyAverageData_ByDataSource_intermediate_test5_sampledLANDFIRE.rds")

# visualizations  ---------------------------
# mapview(dat2_cellID[dat2_cellID$Source == "RAP" & is.finite(dat2_cellID$ShrubCover) & dat2_cellID$Year == 2010,])

## No. of observations per cell
# plots show the maximum # of obs / cell across all years of sampling
# histograms show all values 

averagingFigs <- lapply(X = layerNames, FUN = function(x) {
  
  if (x %in% c("ShrubCover", "TotalHerbaceousCover", "TotalTreeCover","BareGroundCover")) {
    cov_label <- "% Cover"
    cov_title_fig <- paste0("% ",x, ", after spatial averaging process \n(dayMet grid aggregated x 4)")
    cov_title_hist <- paste0("% ",x, ", after spatial averaging process")
  } else {
    cov_label <- "prop."
    cov_title_fig <- paste0(x, ", after spatial averaging process \n(dayMet grid aggregated x 4)")
    cov_title_hist <- paste0(x, ", after spatial averaging process")
  } 
  dat2_n <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>% 
    sf::st_transform(crs(test)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x,"_n"), fun = max, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "max", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  n_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = log(dat2_n), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "log(#of plots)"),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(paste0("Maximum # of plots in a grid cell/year combo. with \n",x, " data (log-transformed)"))+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes((.data[[paste0(x,"_n")]]))) + 
      xlim(c(0,50)) + 
      geom_text(aes(x = 40, y = 1, label = paste0("max No. of plots = ",max(.data[[paste0(x,"_n")]])))) + 
      ggtitle(paste0("Maximum # of plots in a grid/cell combo. \nwith",x," data (curtailed)")),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  ## Cover Values 
  dat2_cov <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = x, fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE)  %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  cov_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = (dat2_cov), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = cov_label),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(cov_title_fig)+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes(.data[[x]])) + 
      #xlim(c(0,50)) + 
      ggtitle(cov_title_hist),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  
  ## SD of cover values within a cell
  dat2_sd <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>%  
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x, "_sd"), fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  sd_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = (dat2_sd), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "F", guide = guide_colorbar(title = "sd"),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(paste0("Stnd. Dev. of ",x," within a grid cell \n(dayMet grid aggregated x 4)"))+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes(.data[[paste0(x, "_sd")]])) + 
      #xlim(c(0,50)) + 
      ggtitle(paste0("% ",x,", after spatial averaging process")),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  
  ## add all together
  allFigs <- ggarrange(
    cov_figs,
    sd_figs,
    n_figs,
    ncol = 1
  )
  return(allFigs)
}
)

# save figures

bitmap(file = "./Figures/CoverDatFigures/SpatiallyAveragedCoverData_filteredRAP_LF.bmp", 
       width = 14, height = 100, res = 200)
ggarrange(
  averagingFigs[[1]],
  averagingFigs[[2]],
  averagingFigs[[3]],
  averagingFigs[[4]],
  averagingFigs[[5]],
  averagingFigs[[6]],
  averagingFigs[[7]],
  averagingFigs[[8]],
  averagingFigs[[9]],
  ncol = 1
)
dev.off()
