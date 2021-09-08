library(tidyverse)

# load travel distances
commute_dists <- read_rds("~/PHEV Small Projects/Alan_TT_TD/Data/all_counties_tract_travel_distance.rds")

# load lodes dataset to see which distances we need to care about
lodes_ca_tracts <- read_rds("Data/ca_lodes_tract.rds")

# subset distances
commute_dists_subset <- commute_dists %>% 
  select(h_tract = F_GEOID, w_tract = T_GEOID, d_meters_network) %>% 
  inner_join(lodes_ca_tracts, by = c("h_tract", "w_tract"))

# save the result
commute_dists_subset %>% 
  write_rds("Data/ca_tract_jobs_dists.rds", compress = "gz")
