library(tidyverse)
library(sf)

# county info
counties <- read_sf("~/GIS DataBase/CA_Counties_polygon/CA_Counties_TIGER2016.shp") %>% 
  select(county_geoid = GEOID, county = NAME)

county_names <- st_drop_geometry(counties)

# load tract-level interpolations
total_zevs <- read_csv("Data/california_total_zevs_interpolation_yr_Sep08.csv",
                       col_types = cols(tract = "c", .default = "d"))

households <- read_csv("Data/california_households_interpolation_yr_Sep08.csv",
                       col_types = cols(tract = "c", .default = "d"))

mods_params_wide <- read_csv("Data/california_charging_interpolation_yr_Sep08.csv",
                             col_types = cols(tract = "c", .default = "d"))

# load tract-level vehicle totals 
tract_vehs_hhs <- read_csv("Data/tract_vehicles_households.csv",
                           col_types = cols_only(tract = "c", 
                                                 total_tract_households = "d"))

county_tot_hhs <- tract_vehs_hhs %>% 
  mutate(county_geoid = str_sub(tract, end = 5)) %>% 
  with_groups(county_geoid, summarize, 
              total_county_households = sum(total_tract_households))

# convert household number from % with ZEv to total with ZEV
households_n <- households %>% 
  left_join(tract_vehs_hhs, by = "tract") %>% 
  mutate(across(zevhhs_yr_int:zevhhs_yr_pw6, ~ . * total_tract_households)) %>% 
  select(-total_tract_households)

# summarize to county level
tract_to_county <- function(tract_level_data) {
  tract_level_data %>% 
    mutate(county_geoid = str_sub(tract, end = 5), .keep = "unused") %>% 
    left_join(county_names, by = "county_geoid") %>% 
    group_by(county, county_geoid) %>% 
    summarize(across(everything(), sum), .groups = "drop")
}

# convert to county and write
total_zevs %>% 
  tract_to_county() %>% 
  write_csv("Data/california_total_zevs_interpolation_yr_Sep08_county.csv")

households_n %>% 
  tract_to_county() %>% 
  # convert back to % by dividing by total county households
  left_join(county_tot_hhs, by = "county_geoid") %>% 
  mutate(across(zevhhs_yr_int:zevhhs_yr_pw6, ~ . / total_county_households)) %>% 
  write_csv("Data/california_households_interpolation_yr_Sep08_county_corrected.csv")

mods_params_wide %>% 
  tract_to_county() %>% 
  write_csv("Data/california_charging_interpolation_yr_Sep08_county.csv")  

# simplify county boundaries
counties %>% 
  st_transform(3310) %>% 
  st_simplify(TRUE, 10) %>% 
  st_transform(st_crs(counties)) %>% 
  write_sf("Data/spatial_data/california_counties_simplified.shp")

