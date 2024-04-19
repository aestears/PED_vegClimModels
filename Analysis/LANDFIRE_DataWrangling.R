#///////////////////
# Importing and exploring potential data sources - LANDFIRE reference data
# Alice Stears
# 04/15/2024
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(mdbr)

# Read in Data ------------------------------------------------------------
# there is an Microsoft access database for each of six regions of the US try
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
nc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
  #This table contains location information for each sampled unit.
  #Table 5: Sample unit location information
nc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
ne_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
#This table contains location information for each sampled unit.
#Table 5: Sample unit location information
ne_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")

# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
nw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
#This table contains location information for each sampled unit.
#Table 5: Sample unit location information
nw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
sc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
#This table contains location information for each sampled unit.
#Table 5: Sample unit location information
sc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")

# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
se_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
#This table contains location information for each sampled unit.
#Table 5: Sample unit location information
se_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
sw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb", 
                         table = "dtStands")
# get site locations: dtPoints
#This table contains location information for each sampled unit.
#Table 5: Sample unit location information
sw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb", 
                           table = "dtPoints")

plot(nc_plots$Lat, nc_plots$Long)
mapview(nc_plots, xcol = "Long", ycol = "Lat", crs = "EPSG:4326", map.types = "OpenStreetMap")
