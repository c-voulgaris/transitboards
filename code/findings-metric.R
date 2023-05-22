## For each block group in the service area:

# Percent change in transit frequency
# Median income
# Percent non-white
# Percent with no bachelor's degree

library(tidyverse)
library(sf)
library(tidytransit)
library(here)
library(tidycensus)

here("helper-functions",
     "gtfs-helpers.R") |>
  source()

options(tigris_use_cache = TRUE)

vars <- c(med_inc_ = "B19013_001",
          total_race_ = "B03002_001",
          nh_white_ = "B03002_003",
          school_total_ = "B15003_001",
          bachelor_ = "B15003_022",
          master_ = "B15003_023",
          profess_ = "B15003_024",
          doctorate_ = "B15003_025")

summary_table <- tibble(City = "first",
                        `Income Correlation` = 9,
                        `Income Correlation p-value` = 9,
                        `Race Correlation` = 9,
                        `Race Correlation p-value` = 9,
                        `Education Correlation` = 9,
                        `Education Correlation p-value` = 9,
                        `Vulnerability Correlation` = 9,
                        `Vulnerability Correlation p-value` = 9) |>
  filter(`Income Correlation` < 1)

comparison <- tibble(city = "first", 
                     income_z = 9, 
                     non_white_z = 9, 
                     not_bach_z = 9, 
                     v_index = 9, 
                     pct_change = 9) |>
  filter(income_z < 1)

places <- here("Networks") |>
  list.dirs(recursive = FALSE,
            full.names = FALSE)

for(i in 1:length(places)) {
  
  state = str_extract(places[i], '(?<=,\\s).+$') |>
    str_split("-") |>
    unlist()
  
  bgs <- get_acs(geography = "cbg", state = state, variables = vars,
                 output = "wide", geometry = TRUE) |>
    st_transform("WGS84") |>
    mutate(pct_non_white = 1 - (nh_white_E/total_race_E),
           pct_not_bach = 1 - 
             ((bachelor_E + master_E + profess_E + doctorate_E)/
                school_total_E)) |>
    mutate(income_z = 
             (med_inc_E - mean(med_inc_E, na.rm = TRUE))/
             sd(med_inc_E, na.rm = TRUE),
           non_white_z = 
             (pct_non_white - mean(pct_non_white, na.rm = TRUE))/
             sd(pct_non_white, na.rm = TRUE),
           not_bach_z = 
             (pct_not_bach - mean(pct_not_bach, na.rm = TRUE))/
             sd(pct_not_bach, na.rm = TRUE)) |>
    mutate(v_index = (income_z + non_white_z + not_bach_z)/3) |>
    select(GEOID, income_z, non_white_z, not_bach_z, v_index)
  
  before_feeds <- here("Networks",
                places[i],
                "1Feb") |>
    list.files(recursive = FALSE,
              full.names = FALSE)
  
  after_feeds <- here("Networks",
                       places[i],
                       "31May") |>
    list.files(recursive = FALSE,
               full.names = FALSE)
  
  before_stop_freq <- after_stop_freq <- tibble(stop_id = "none",
                                                n_times_daily = 0,
                                                lat = 0,
                                                lon = 0) |>
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
    filter(n_times_daily > 0)
  
  for (j in 1:length(before_feeds)) {
    this_before_stop_freq <- here("Networks",
                              places[i],
                              "1Feb",
                              before_feeds[j]) |>
      read_gtfs() |>
      fn_daily_stop_freq()
    
    before_stop_freq <- rbind(before_stop_freq, this_before_stop_freq)
  }
  for (j in 1:length(after_feeds)) {
    this_after_stop_freq <- here("Networks",
                            places[i],
                            "31May",
                            after_feeds[j]) |>
      read_gtfs() |>
      fn_daily_stop_freq() 
    
    after_stop_freq <- rbind(after_stop_freq, this_after_stop_freq)
  }
  
  before_stop_freq <- before_stop_freq  |>
    st_join(bgs)|>
    st_drop_geometry() |>
    group_by(GEOID, income_z, non_white_z, not_bach_z, v_index) |>
    summarise(before_freq = sum(n_times_daily))

  after_stop_freq <- after_stop_freq |>  
    st_join(bgs) |>
    st_drop_geometry() |>
    group_by(GEOID, income_z, non_white_z, not_bach_z, v_index) |>
    summarise(after_freq = sum(n_times_daily))
  
  this_comparison <- full_join(before_stop_freq, after_stop_freq) |>
    mutate(pct_change = (after_freq - before_freq) / before_freq) |>
    mutate(city = places[i]) |>
    select(city, income_z, non_white_z, not_bach_z, v_index, before_freq, after_freq, pct_change) |>
    filter(!is.na(income_z),
           !is.na(non_white_z),
           !is.na(not_bach_z),
           !is.na(pct_change))

  inc_cor <- cor.test(~income_z+pct_change, this_comparison)
  race_cor <- cor.test(~non_white_z+pct_change, this_comparison)
  educ_cor <- cor.test(~not_bach_z+pct_change, this_comparison)
  v_i_cor <- cor.test(~v_index+pct_change, this_comparison)
  
  this_summary <- tibble(City = places[i],
                         `Income Correlation` = inc_cor$estimate,
                         `Income Correlation p-value` = inc_cor$p.value,
                         `Race Correlation` = race_cor$estimate,
                         `Race Correlation p-value` = race_cor$p.value,
                         `Education Correlation` = educ_cor$estimate,
                         `Education Correlation p-value` = educ_cor$p.value,
                         `Vulnerability Correlation` = v_i_cor$estimate,
                         `Vulnerability Correlation p-value` = v_i_cor$p.value) 
  

  comparison <- rbind(comparison, this_comparison)
    
  summary_table <- rbind(summary_table, this_summary)
}

upper_bound <- quantile(comparison$pct_change, probs = 0.99)
comparison <- comparison |> filter(pct_change < upper_bound)

vi_plot <- ggplot(comparison,
       aes(x = v_index,
           y = pct_change)) +
  geom_point(size = 0.1,
             alpha = 0.5,
             color = "lightblue") +
  stat_smooth(method = "lm",
              color = "black",
              fill = "black") +
  facet_wrap(~ city, ncol = 6) +
  geom_abline(slope = 0, intercept = 0,
              color = "gray",
              linetype = "dashed")

vi_plot

inc_plot <- ggplot(comparison,
                  aes(x = income_z,
                      y = pct_change)) +
  geom_point(size = 0.1,
             alpha = 0.5,
             color = "lightblue") +
  stat_smooth(method = "lm",
              color = "black",
              fill = "black") +
  facet_wrap(~ city, ncol = 6) +
  geom_abline(slope = 0, intercept = 0,
              color = "gray",
              linetype = "dashed")

inc_plot

race_plot <- ggplot(comparison,
                   aes(x = non_white_z,
                       y = pct_change)) +
  geom_point(size = 0.1,
             alpha = 0.5,
             color = "lightblue") +
  stat_smooth(method = "lm",
              color = "black",
              fill = "black") +
  facet_wrap(~ city, ncol = 6) +
  geom_abline(slope = 0, intercept = 0,
              color = "gray",
              linetype = "dashed")

race_plot

here()
ggsave()

write_csv(comparison, here("Data", "bg_freqency.csv"))
