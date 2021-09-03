library(tidyverse)

# California LODES totals (block level at start)
lodes_ca_raw <- read_csv("Data/ca_od_main_JT00_2018.csv.gz",
                         col_types = cols_only(w_geocode = col_character(),
                                               h_geocode = col_character(),
                                               S000      = col_integer()),
                         lazy = FALSE)



# grab the census tract portion of each geocode
lodes_ca_raw_with_tracts <- lodes_ca_raw %>% 
  mutate(h_tract = str_sub(h_geocode, 1, 11),
         w_tract = str_sub(w_geocode, 1, 11))

# summarize to Tract-Tract totals
lodes_ca_tracts <- lodes_ca_raw_with_tracts %>% 
  with_groups(c(h_tract, w_tract), summarize, tot_jobs = sum(S000))

# save the result
lodes_ca_tracts %>% 
  write_rds("Data/ca_lodes_tract.rds", compress = "gz")

# compute basic commute distances? or do I have those elsewhere ...