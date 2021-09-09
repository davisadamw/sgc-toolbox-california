library(tidyverse)
library(sf)

census_tracts_raw <- read_sf("Data/spatial_data/tl_2019_06_tract.shp") %>% 
  select(GEOID, geometry)

# simplify @ 10m
census_tracts_simplified <- census_tracts_raw %>% 
  st_transform(3310) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 10) %>% 
  st_transform(st_crs(census_tracts_raw))

census_tracts_simplified %>% 
  write_sf("Data/spatial_data/california_tracts_simplified.shp")
