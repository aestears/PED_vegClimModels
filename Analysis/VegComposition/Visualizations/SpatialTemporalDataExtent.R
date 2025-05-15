#///////////////////
# Visualizing the spatial and temporal extent of the cover data
# Alice Stears
# 1/07/2025
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(ggpubr)
# load data  -------------------------------------------------------------- use
# data before climate values are added, so that we can tell how much of
# potential data we're losing when we use different climate cutoffs
vegCovDat <- readRDS("./Data_processed/CoverData/dataForAnalysis_fireRemoved.rds")

vegCovDat <- vegCovDat %>% 
  rename(StateUnitCode = SttUntC, # fix names 
         PlotCondition = PltCndt,
         ShrubCover = ShrbCvr,
         TotalTreeCover = TtlTrCv, 
         TotalHerbaceousCover = TtlHrbC, 
         CAMCover  = CAMCovr, 
         BareGroundCover = BrGrndC, 
         ForbCover_prop = FrbCvr_,
         C3Cover_prop = C3GrmC_,
         C4Cover_prop = C4GrmC_,
         BroadleavedTreeCover_prop = AngTrC_,
         NeedleLeavedTreeCover_prop = CnfTrC_,
         #AnnualHerbaceousCover = AnnlHGC,
         #PerennialHerbaceousCover = PrnnHGC,,
         #C3GramCover = C3GrmCv,
         #C4GramCover = C4GrmCv,
         #BroadLeavedTreeCover = AngTrCv,
         #NeedleLeavedTreeCover = CnfTrCv,
         #TotalTreeCover = TtlTrCv,
         TotalTreeBasalArea_m2 = TrBsA_2,
         BareGroundCover = BrGrndC,
         LitterCover = LttrCvr,
         LitterDepth = LttrDpt,
         #ForbCover_prop = HrbCvr_,
         #C3GramCover_prop = C3GrmC_,
         #C4GramCover_prop = C4GrmC_,
         #BroadLeavedTreeCover_prop = AngTrC_,
         #NeedleLeavedTreeCover_prop = CnfTrC_
  ) 


## calculate the number of observations for each 'scenario' 
# scenario 1 - climate window of 10-30 years (data from 1990 onward)
dat_s1 <- vegCovDat %>% 
  st_drop_geometry() %>% 
  dplyr::select(Source, Year, ShrubCover, TotalTreeCover, CAMCover, TotalHerbaceousCover, BareGroundCover,
                BroadleavedTreeCover_prop, NeedleLeavedTreeCover_prop, C4Cover_prop, C3Cover_prop, ForbCover_prop) %>%
  #drop_na()  %>% 
  filter(Year >=1990)  %>% 
  group_by(Source)%>% 
  dplyr::summarize(ShrubCover = sum(!is.na(ShrubCover)) ,
                   TotalTreeCover = sum(!is.na(TotalTreeCover)),
                   CAMCover = sum(!is.na(CAMCover)),
                   TotalHerbaceousCover = sum(!is.na(TotalHerbaceousCover)),
                   BareGroundCover = sum(!is.na(BareGroundCover)),
                   BroadleavedTreeCover_prop = sum(!is.na(BroadleavedTreeCover_prop)),
                   NeedleLeavedTreeCover_prop = sum(!is.na(NeedleLeavedTreeCover_prop)),
                   C4Cover_prop = sum(!is.na(C4Cover_prop)),
                   C3Cover_prop = sum(!is.na(C3Cover_prop)),
                   ForbCover_prop = sum(!is.na(ForbCover_prop))
  ) %>% 
  pivot_longer(cols = ShrubCover:ForbCover_prop, names_to = "variable", values_to = "s1")

# scenario 2 - climate window of 15-30 years (data from 1990 onward)
dat_s2 <- vegCovDat %>% 
  st_drop_geometry() %>% 
  dplyr::select(Source, Year, ShrubCover, TotalTreeCover, CAMCover, TotalHerbaceousCover, BareGroundCover,
                BroadleavedTreeCover_prop, NeedleLeavedTreeCover_prop, C4Cover_prop, C3Cover_prop, ForbCover_prop) %>%
  #drop_na()  %>% 
  filter(Year >=1995)  %>% 
  group_by(Source) %>% 
  dplyr::summarize(ShrubCover = sum(!is.na(ShrubCover)) ,
                   TotalTreeCover = sum(!is.na(TotalTreeCover)),
                   CAMCover = sum(!is.na(CAMCover)),
                   TotalHerbaceousCover = sum(!is.na(TotalHerbaceousCover)),
                   BareGroundCover = sum(!is.na(BareGroundCover)),
                   BroadleavedTreeCover_prop = sum(!is.na(BroadleavedTreeCover_prop)),
                   NeedleLeavedTreeCover_prop = sum(!is.na(NeedleLeavedTreeCover_prop)),
                   C4Cover_prop = sum(!is.na(C4Cover_prop)),
                   C3Cover_prop = sum(!is.na(C3Cover_prop)),
                   ForbCover_prop = sum(!is.na(ForbCover_prop))
  ) %>% 
  pivot_longer(cols = ShrubCover:ForbCover_prop, names_to = "variable", values_to = "s2")

# scenario 3 - climate window of 20-30 years (data from 1990 onward)
dat_s3 <- vegCovDat %>% 
  st_drop_geometry() %>% 
  dplyr::select(Source, Year, ShrubCover, TotalTreeCover, CAMCover, TotalHerbaceousCover, BareGroundCover,
                BroadleavedTreeCover_prop, NeedleLeavedTreeCover_prop, C4Cover_prop, C3Cover_prop, ForbCover_prop) %>%
  #drop_na()  %>% 
  filter(Year >=2000)  %>% 
  group_by(Source) %>% 
  dplyr::summarize(ShrubCover = sum(!is.na(ShrubCover)) ,
                   TotalTreeCover = sum(!is.na(TotalTreeCover)),
                   CAMCover = sum(!is.na(CAMCover)),
                   TotalHerbaceousCover = sum(!is.na(TotalHerbaceousCover)),
                   BareGroundCover = sum(!is.na(BareGroundCover)),
                   BroadleavedTreeCover_prop = sum(!is.na(BroadleavedTreeCover_prop)),
                   NeedleLeavedTreeCover_prop = sum(!is.na(NeedleLeavedTreeCover_prop)),
                   C4Cover_prop = sum(!is.na(C4Cover_prop)),
                   C3Cover_prop = sum(!is.na(C3Cover_prop)),
                   ForbCover_prop = sum(!is.na(ForbCover_prop))
  ) %>% 
  pivot_longer(cols = ShrubCover:ForbCover_prop, names_to = "variable", values_to = "s3")

# scenario 4 - climate window of 25-30 years (data from 1990 onward)
dat_s4 <- vegCovDat %>% 
  st_drop_geometry() %>% 
  dplyr::select(Source, Year, ShrubCover, TotalTreeCover, CAMCover, TotalHerbaceousCover, BareGroundCover,
                BroadleavedTreeCover_prop, NeedleLeavedTreeCover_prop, C4Cover_prop, C3Cover_prop, ForbCover_prop) %>%
  #drop_na()  %>% 
  filter(Year >=2005)  %>% 
  group_by(Source) %>% 
  dplyr::summarize(ShrubCover = sum(!is.na(ShrubCover)) ,
                   TotalTreeCover = sum(!is.na(TotalTreeCover)),
                   CAMCover = sum(!is.na(CAMCover)),
                   TotalHerbaceousCover = sum(!is.na(TotalHerbaceousCover)),
                   BareGroundCover = sum(!is.na(BareGroundCover)),
                   BroadleavedTreeCover_prop = sum(!is.na(BroadleavedTreeCover_prop)),
                   NeedleLeavedTreeCover_prop = sum(!is.na(NeedleLeavedTreeCover_prop)),
                   C4Cover_prop = sum(!is.na(C4Cover_prop)),
                   C3Cover_prop = sum(!is.na(C3Cover_prop)),
                   ForbCover_prop = sum(!is.na(ForbCover_prop))
  ) %>% 
  pivot_longer(cols = ShrubCover:ForbCover_prop, names_to = "variable", values_to = "s4")

# calculate % of data lost for each group across each scenario
dat_sAll <- left_join(dat_s1, dat_s2) %>% 
  left_join(dat_s3) %>% 
  left_join(dat_s4) %>% 
  # compare scenario 1 to scenario 2
  mutate(percRemaining_s1tos2 = round(s2/s1 * 100,1),
         percRemaining_s1tos3 = round(s3/s1 * 100,1),
         percRemaining_s1tos4 = round(s4/s1 * 100,1)) %>% 
  filter(!is.nan(percRemaining_s1tos2))


# make figures (scenario 1: most data, using minimum of 10 years for cliamte data------------------------------------------------------------
# shrub cover
(shrubYears_s1 <- vegCovDat %>% 
  dplyr::select(Source, Year, ShrubCover) %>%
  drop_na()  %>% 
   filter(Year >=1990) %>% 
ggplot() + 
  geom_vline(aes(xintercept = 1995), col = "orange") + 
  geom_vline(aes(xintercept = 2000), col = "green") + 
  geom_vline(aes(xintercept = 2005), col = "blue") + 
  geom_text(data = dat_sAll[dat_sAll$variable == "ShrubCover",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
  geom_text(data = dat_sAll[dat_sAll$variable == "ShrubCover",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
  geom_text(data = dat_sAll[dat_sAll$variable == "ShrubCover",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
  geom_density(aes(Year)) + 
  facet_wrap(~Source) + 
  ggtitle("Years when we have shrub cover data, by data source") + 
  theme_classic())

# forest cover
(forestYears_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, TotalTreeCover) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalTreeCover",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalTreeCover",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalTreeCover",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have tree cover data, by data source") + 
    theme_classic())

# CAM cover 
(camYears_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, CAMCover) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "CAMCover",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "CAMCover",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "CAMCover",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source, ncol = 1) + 
    ggtitle("Years when we have CAM cover data, by data source") + 
    theme_classic())

# herb + grass cover
(herbaceousYears_S1 <- vegCovDat %>% 
  dplyr::select(Source, Year, TotalHerbaceousCover) %>%
  drop_na()  %>% 
    filter(Year >=1990) %>% 
  ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalHerbaceousCover",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalHerbaceousCover",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "TotalHerbaceousCover",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
  geom_density(aes(Year)) + 
  facet_wrap(~Source) + 
  ggtitle("Years when we have total herbaceous cover data, by data source") + 
    theme_classic())

# broad leaved tree cover
(broadLeavedTreeYears_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, BroadleavedTreeCover_prop) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "BroadleavedTreeCover_prop",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "BroadleavedTreeCover_prop",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "BroadleavedTreeCover_prop",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Broad leaved Tree Cover proportion data, by data source") + 
    theme_classic())

# needle leaved tree cover
(needleLeavedTreeYears_S1 <- vegCovDat %>% 
    dplyr::select(Source, Year, NeedleLeavedTreeCover_prop) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "NeedleLeavedTreeCover_prop",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "NeedleLeavedTreeCover_prop",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "NeedleLeavedTreeCover_prop",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Needle leaved Tree Cover proportion data, by data source") + 
    theme_classic())

# forb cover proportion
(forbYears_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, ForbCover_prop) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "ForbCover_prop",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "ForbCover_prop",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "ForbCover_prop",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have Forb Cover proportion data, by data source") + 
    theme_classic())


# c3 proportion
(c3Years_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, C3Cover_prop) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "C3Cover_prop",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "C3Cover_prop",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "C3Cover_prop",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have C3 grass Cover proportion data, by data source") + 
    theme_classic())


# c3 proportion
(c4Years_s1 <- vegCovDat %>% 
    dplyr::select(Source, Year, C4Cover_prop) %>%
    drop_na()  %>% 
    filter(Year >=1990) %>% 
    ggplot() + 
    geom_vline(aes(xintercept = 1995), col = "orange") + 
    geom_vline(aes(xintercept = 2000), col = "green") + 
    geom_vline(aes(xintercept = 2005), col = "blue") + 
    geom_text(data = dat_sAll[dat_sAll$variable == "C4Cover_prop",], aes(x = 1997, y = 0.15, label = percRemaining_s1tos2), col = "orange") +
    geom_text(data = dat_sAll[dat_sAll$variable == "C4Cover_prop",], aes(x = 2002, y = 0.15, label = percRemaining_s1tos3), col = "green") +
    geom_text(data = dat_sAll[dat_sAll$variable == "C4Cover_prop",], aes(x = 2007, y = 0.15, label = percRemaining_s1tos4), col = "blue") +
    geom_density(aes(Year)) + 
    facet_wrap(~Source) + 
    ggtitle("Years when we have C4 grass Cover proportion data, by data source") + 
    theme_classic())



(plots <- ggarrange(ggarrange(shrubYears_s1, forestYears_s1, camYears_s1, herbaceousYears_S1, ncol = 1),
                    ggarrange(
                    broadLeavedTreeYears_s1, needleLeavedTreeYears_S1, forbYears_s1, c3Years_s1, c4Years_s1, ncol = 1)
) %>% 
    ggpubr::annotate_figure(top = "Frequency of Veg. Cover Data Across Years by Source and Functional Group
                          The intial extent of the period for determining climate variables was 10-30 years, which means 
                          we can use data from 1990 onward. The vertical lines indicate the cutoff points for longer intervals 
                          for calculating climate (orange: 15-30 years; green: 20-30 years, blue: 25-40 years). Numbers next 
                          to each vertical line indicate the percentage of the maximum data (10-30 year climate interval) that 
                          is lost with a given new climate interval.
                            " 
                              )
)



## save figure
bitmap(file = "./Figures/CoverDatFigures/TemporalExtentOfCoverData.bmp", 
       width = 18, height = 15, res = 200)
plots
dev.off()
# save images -------------------------------------------------------------


