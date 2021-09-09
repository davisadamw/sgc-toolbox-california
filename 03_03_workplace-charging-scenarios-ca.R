library(tidyverse)
library(evworkplace)
library(broom)

# load adoption scenario for California
# ... for now, let's let home charging % vary
# ... in the future, we could edit this to work with modeled charging for CA 
ca_adoption_steps <- read_csv("Data/tract_zevs.csv",
                              lazy = FALSE,
                              col_types = cols_only(year = "i",
                                                    tract = "c",
                                                    total_tract_zevs = "d"))

# california_charging_steps <- read_csv("Data/tract_charging.csv")

# create charging probabilities ... currently only for long range vehicle fraction
charge_probs <-
  crossing(
    workcharge_free = c(TRUE, FALSE),
    long_range_frac = seq(0, 1, by = 0.1),
    homecharge_frac = seq(0.5, 1, by = 0.1)) %>% 
  mutate(charge_probs = pmap(
    list(workcharge_free, long_range_frac, homecharge_frac), 
    ~ make_workcharge_probs(is_workcharge_free = ..1,
                            long_range_wt = ..2,
                            medium_range_wt = (1 - ..2) * 0.8,
                            short_range_wt = (1 - ..2) * 0.2,
                            sfh_homecharge = ..3)
  )) %>% 
  unnest(charge_probs)

charge_probs_abcd <- charge_probs %>% 
  distinct(cmtdist) %>% 
  mutate(cmtdist_abcd = case_when(
    cmtdist < 10 ~ "a",
    cmtdist < 20 ~ "b",
    cmtdist < 30 ~ "c",
    cmtdist < 40 ~ "d"))

# make commute distance interpolator
available_dists <- unique(charge_probs$cmtdist) # unique values of cmtdist
barrier_dists <- (available_dists + lag(available_dists, default = 0))/2 # barriers between these values

commute_interpolator <- approxfun(x = barrier_dists,
                                  y = available_dists,
                                  method = "constant",
                                  rule = 2)

# prepare the workcharging data ####
# load commute totals and approximate distances
commute_details <- read_rds("Data/ca_tract_jobs_dists.rds") %>% 
  # set all missing same-tract distances to 0
  mutate(d_meters_network = if_else(is.na(d_meters_network) & (h_tract == w_tract),
                                    0, d_meters_network)) %>% 
  # match each commute distance to the nearest one for which we have a probability
  mutate(commute_mi = d_meters_network / 1608,
         cmtdist_use = commute_interpolator(commute_mi)) %>% 
  drop_na(commute_mi) %>% 
  with_groups(h_tract, mutate, job_frac = tot_jobs / sum(tot_jobs)) %>% 
  left_join(charge_probs_abcd, by = c("cmtdist_use" = "cmtdist"))

# attach adoption info to commuter info
commuters_by_scenario <- ca_adoption_steps %>%
  left_join(commute_details, by = c("tract" = "h_tract")) %>% 
  mutate(vehs = total_tract_zevs * job_frac) %>% 
  group_by(tract = w_tract, year, cmtdist_abcd) %>% 
  summarize(possible_commuters = sum(vehs),
            .groups = "drop") %>% 
  mutate(possible_commuters = replace_na(possible_commuters, 0)) %>% 
  drop_na(tract)

# separate data for interpolation ... and make model with third degree poly 
commuter_models <- commuters_by_scenario %>% 
  mutate(yearf = year - 2019, .keep = "unused") %>% 
  nest(data = c(possible_commuters, yearf)) %>% 
  mutate(model = map(data, ~ lm(possible_commuters ~ poly(yearf, 3, raw = TRUE), data = .)))

# grab the coefficients
mods_params <- commuter_models %>% 
  mutate(tidied = map2(model, data, ~ tidy(.x, data = .y)),
         .keep = "unused") %>% 
  unnest(tidied)

# so ... for output, we need the 0th-3rd power coefficients for each model, in wide format
mods_params_wide <- mods_params %>% 
  mutate(term_name = if_else(term == "(Intercept)", "int", paste0("pw", str_sub(term, -1)))) %>% 
  pivot_wider(id_cols = tract,
              names_from = c(cmtdist_abcd, term_name),
              values_from = estimate,
              values_fill = 0) %>% 
  rename_with(~ paste("commute_yr", ., sep = "_"), -tract)

# finally ... write the results to disk
mods_params_wide %>% 
  write_csv("Data/california_charging_interpolation_yr_Sep08.csv")

