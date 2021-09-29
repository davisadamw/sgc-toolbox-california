library(tidyverse)
library(sf)

all_years <- 2021:2040

# total vehicles in area
all_vehs <- read_csv("Data/tract_vehicles_households.csv",
                     col_types = cols_only(tract = "c", 
                                           total_tract_vehicles = "d"))

# load adoption totals
total_zevs <- read_csv("Data/pow5_total_zev_by_tract_interp.csv",
                       col_types = cols(tract = "c", .default = "d"))

# county definitions
county_regions <- readxl::read_excel("Data/CaliforniaCountyGrouping_rewts.xlsx") %>% 
  mutate(geoid_start = paste0("06", str_pad(FIPS, 3, "left", "0")))

# create initial weights by year
mult_x_year <- crossing(
  Initial_Mult = county_regions$Initial_Mult,
  year = all_years) %>% 
  mutate(year_x_21 = (year - 2021) / max(year - 2021),
         basis_mult = Initial_Mult * (1 - year_x_21) + year_x_21) %>% 
  select(Initial_Mult, year, basis_mult)

# apply interpolation
tract_zev_years <- total_zevs %>% 
  crossing(year = all_years) %>% 
  mutate(yuse = year - 2020,
         zevs = int + pw1 * yuse + pw2 * yuse^2 + pw3 * yuse^3 + 
           pw4 * yuse^4 + pw5 * yuse^5,
         zevs = pmax(zevs, 0)) %>% 
  select(tract, year, zevs)

# apply weights (increase ZEVs in places with high early adoption rates)
tract_zev_years_wt <- tract_zev_years %>% 
  mutate(geoid_start = str_sub(tract, 1, 5)) %>% 
  left_join(county_regions, 
            by = "geoid_start") %>% 
  left_join(mult_x_year, by = c("Initial_Mult", "year")) %>% 
  mutate(zevs_adj = zevs * basis_mult) %>% 
  with_groups(year, mutate,
              zevs_final = zevs_adj * sum(zevs) / sum(zevs_adj)) %>% 
  select(tract, year, geoid_start, County, Region, zevs = zevs_final)

zev_years <- tract_zev_years_wt %>% 
  with_groups(year, summarize, tot_zevs = sum(zevs)) %>% 
  mutate(zev_pct = tot_zevs / 25311775,
         yr_label = glue::glue("{year} ({scales::percent(zev_pct, 1)})"))


# target years = 2021, 2023, 2028, 2030
targ_scens <- tract_zev_years_wt %>% 
  left_join(all_vehs, by = "tract") %>% 
  mutate(veh_frac = zevs / total_tract_vehicles)

# load spatial data
california_polygons <- read_sf("Data/spatial_data/california_tracts_simplified.shp") %>% 
  st_transform(3310) %>% 
  rename(tract = GEOID)

# attach spatial info
targ_scens_spatial <- targ_scens %>% 
  filter(year %% 5 == 0) %>% 
  left_join(zev_years, by = "year") %>% 
  left_join(california_polygons, by = "tract") %>% 
  st_as_sf(sf_column_name = "geometry")

targ_scens_spatial %>% 
  ggplot(aes(fill = veh_frac)) +
  facet_grid(cols = vars(yr_label)) +
  geom_sf(color = NA) +
  scale_fill_fermenter("EVs as a % of all vehicles",
                       palette = "Purples",
                       direction = 0,
                       labels = scales::percent_format(1),
                       breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 1)) +
  #theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("all_years_asilomar.png", height = 5, width = 12)

# also get regional vehicle %

targ_scens %>% 
  with_groups(c(Region, year), summarize,
              veh_frac = sum(zevs) / sum(total_tract_vehicles)) %>% 
  pivot_wider(names_from = Region, values_from = veh_frac) %>% 
  relocate(`Rest of CA`, .after = last_col()) %>% 
  clipr::write_clip()

         