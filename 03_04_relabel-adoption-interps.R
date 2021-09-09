library(tidyverse)

# total zevs interps
total_zevs <- read_csv("Data/pow5_total_zev_by_tract_interp.csv",
                       col_types = cols(tract = "c", .default = "d")) %>% 
  rename_with(~ paste("totzevs_yr", ., sep = "_"), -tract)

# percent of zev households interps
households <- read_csv("Data/pow6_perc_of_households_by_tract_interp.csv",
                       col_types = cols(tract = "c", .default = "d")) %>% 
  rename_with(~ paste("zevhhs_yr", ., sep = "_"), -tract)

# and save the results
total_zevs %>% 
  write_csv("Data/california_total_zevs_interpolation_yr_Sep08.csv")

households %>% 
  write_csv("Data/california_households_interpolation_yr_Sep08.csv")