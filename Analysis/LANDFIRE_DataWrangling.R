#///////////////////
# Importing and exploring potential data sources - LANDFIRE reference data
# Alice Stears
# 04/15/2024
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(mdbr)
library(mapview)

#Read in Data ------------------------------------------------------------
# there is an Microsoft access database for each of six regions of the US try
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
# nc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
#   #This table contains location information for each sampled unit.
#   #Table 5: Sample unit location information
# nc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# nc_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# nc_dat <- nc_dat %>%
#   left_join(nc_plots) %>% 
#   left_join(nc_source)
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# ne_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# ne_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# ne_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# ne_dat <- ne_dat %>%
#   left_join(ne_plots) %>% 
#   left_join(ne_source)
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# nw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# #get site locations: dtPoints This table contains location information for each
# #sampled unit. Table 5: Sample unit location information doesn't have location
# #information for a lot of plots?? why?? Just have an 'x' in the location method
# #column, which means their location is unknown? not very helpful...
# nw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# nw_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# 
# # add plot data to the data
# nw_dat <- nw_dat %>%
#   left_join(nw_plots)%>% 
#   left_join(nw_source, by = "EventID")
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# sc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# sc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# sc_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# 
# #
# # add plot data to the data
# sc_dat <- sc_dat %>%
#   left_join(sc_plots) %>% 
#   left_join(sc_source)
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# se_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# se_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# se_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# se_dat <- se_dat %>%
#   left_join(se_plots) %>% 
#   left_join(se_source)
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# sw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# sw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# sw_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# sw_dat <- sw_dat %>%
#   left_join(sw_plots) %>% 
#   left_join(sw_source)
# 
# ## put all the data into one table
# dat <- rbind(nc_dat, ne_dat, nw_dat, sc_dat, se_dat, sw_dat) %>%
#   filter(!is.na(Long))
# 
# # get codes for "SourceID"
# LUsource <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
#                               table = "lutdtVisitsSourceID")
# # add source information to the d.f
# dat <- dat %>% 
#   left_join(LUsource, by = "SourceID")
# 
# ## save data for later
# write.csv(dat, file = "./data/LANDFIRE_LFRDB/coverDat_all.csv", row.names = FALSE)

dat <- read.csv("./data/LANDFIRE_LFRDB/coverDat_all.csv")

# what are the data sources? 
table(dat$Protocol)

mapview(dat, xcol = "Long", ycol = "Lat", crs = "EPSG:4326", map.types = "OpenStreetMap")
