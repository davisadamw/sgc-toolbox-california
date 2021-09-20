library(tidyverse)
library(sf)

# total vehicles in area
all_vehs <- read_csv("Data/tract_vehicles_households.csv",
                     col_types = cols_only(tract = "c", 
                                           total_tract_vehicles = "d"))

# load adoption totals
total_zevs <- read_csv("Data/pow5_total_zev_by_tract_interp.csv",
                       col_types = cols(tract = "c", .default = "d"))

# apply interpolation
tract_zev_years <- total_zevs %>% 
  crossing(year = 2021:2030) %>% 
  mutate(yuse = year - 2020,
         zevs = int + pw1 * yuse + pw2 * yuse^2 + pw3 * yuse^3 + 
           pw4 * yuse^4 + pw5 * yuse^5) %>% 
  select(tract, year, zevs)

zev_years <- tract_zev_years %>% 
  with_groups(year, summarize, tot_zevs = sum(zevs))

# target years = 2021, 2023, 2028, 2030
targ_scens <- tract_zev_years %>% 
  mutate(scenario = case_when(year == 2021 ~ "0.5 million",
                              year == 2023 ~ "1 million",
                              year == 2028 ~ "3 million",
                              year == 2030 ~ "5 million",
                              TRUE         ~ NA_character_)) %>% 
  filter(!is.na(scenario)) %>% 
  left_join(all_vehs, by = "tract") %>% 
  mutate(veh_frac = zevs / total_tract_vehicles)

# load spatial data
california_polygons <- read_sf("Data/california_tracts_simplified/california_tracts_simplified.shp") %>% 
  rename(tract = GEOID)

filenames <- tibble(scenario = c("0.5 million", "1 million", "3 million", "5 million"),
                    filename = c("500k_map.png", "1M_map.png", "3M_map.png", "5M_map.png"))

all_scens <- targ_scens %>% 
  select(tract, scenario, veh_frac) %>% 
  mutate(veh_frac = pmax(veh_frac, 0)) %>% 
  left_join(filenames, by = "scenario") %>% 
  left_join(california_polygons, by = "tract") %>% 
  st_as_sf()


map_basic <- all_scens %>% 
  ggplot(aes(fill = veh_frac)) +
  scale_fill_fermenter("EVs as a % of all vehicles",
                       palette = "Purples",
                       direction = 0,
                       labels = scales::percent,
                       breaks = c(0, 0.02, 0.04, 0.09, 0.16, 0.5)) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8),
        legend.background = element_rect(color = "black", size = 0.5))

# NOTE THIS DOESN"T WORK ONCE YOU RUN OUT OF LOWER LEVELS FOR SOME REASON

mapper <- function(scenario_val, filename) {
  map_basic +
    geom_sf(data = ~ filter(., scenario == scenario_val), color = NA) +
    ggtitle(glue::glue("{scenario_val} EVs in California"))

  ggsave(filename)
}

map2(filenames$scenario,
     filenames$filename,
     mapper)


