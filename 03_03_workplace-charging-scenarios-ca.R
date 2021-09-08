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

#### STOP HERE UNTIL WE FIND COMMUTE DISTANCES ####

# prepare the workcharging data ####
# load commute totals and approximate distances
commute_details <- read_rds("Data/ca_lodes_tract.rds")# %>% 
  # match each commute distance to the nearest one for which we have a probability
  crossing(cmtdist_use = unique(charge_probs$cmtdist)) %>% 
  mutate(cmtdif = abs(cmtdist_use - commute_mi)) %>% 
  with_groups(c(zcta_h, zcta_w), slice_min, order_by = cmtdif, n = 1) %>% 
  select(-cmtdif) %>% 
  left_join(charge_probs_abcd, by = c("cmtdist_use" = "cmtdist"))

# attach adoption info to commuter info
commuters_by_scenario <- bind_rows(interp_model, interp_flat) %>%
  left_join(commute_details, by = c("ZIP" = "zcta_h")) %>% 
  mutate(vehs = tot_adoptions * job_frac) %>% 
  group_by(ZIP = zcta_w, model_type, adoption_frac, cmtdist_abcd) %>% 
  summarize(possible_commuters = sum(vehs),
            .groups = "drop") %>% 
  mutate(possible_commuters = replace_na(possible_commuters, 0)) %>% 
  # small number of commute pairs dont really show up right
  drop_na(ZIP)

# separate data for interpolation ... and make model with third degree poly 
commuter_models <- commuters_by_scenario %>% 
  mutate(af = as.numeric(str_sub(adoption_frac, 1, 3)), .keep = "unused") %>% 
  nest(data = c(possible_commuters, af)) %>% 
  mutate(model = map(data, ~ lm(possible_commuters ~ poly(af, 3, raw = TRUE), data = .)))

# grab the coefficients
mods_params <- commuter_models %>% 
  mutate(tidied = map2(model, data, ~ tidy(.x, data = .y)),
         .keep = "unused") %>% 
  unnest(tidied)

# so ... for output, we need the 0th-3rd power coefficients for each model, in wide format
mods_params_wide <- mods_params %>% 
  mutate(term_name = if_else(term == "(Intercept)", "int", paste0("pw", str_sub(term, -1)))) %>% 
  pivot_wider(id_cols = ZIP,
              names_from = c(model_type, cmtdist_abcd, term_name),
              values_from = estimate,
              values_fill = 0) %>% 
  rename_with(~ paste("commute", ., sep = "_"))

# finally ... write the results to disk
mods_params_wide %>% 
  write_csv("Data/delmarva_charging_interpolation_Aug04.csv")

