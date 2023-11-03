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

comparison <- tibble(city = "first", 
                     income_z = 9, 
                     non_white_z = 9, 
                     not_bach_z = 9, 
                     priv_index = 9, 
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
    mutate(pct_white = nh_white_E/total_race_E,
           pct_bach =  (bachelor_E + master_E + profess_E + doctorate_E)/
                school_total_E) |>
    mutate(income_z = 
             (med_inc_E - mean(med_inc_E, na.rm = TRUE))/
             sd(med_inc_E, na.rm = TRUE),
           white_z = 
             (pct_white - mean(pct_white, na.rm = TRUE))/
             sd(pct_white, na.rm = TRUE),
           bach_z = 
             (pct_bach - mean(pct_bach, na.rm = TRUE))/
             sd(pct_bach, na.rm = TRUE)) |>
    mutate(priv_index = (income_z + white_z + bach_z)/3) |>
    select(GEOID, income_z, white_z, bach_z, priv_index)
  
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
      fn_daily_trip_freq()
    
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
    group_by(GEOID, income_z, white_z, bach_z, priv_index) |>
    summarise(before_freq = sum(n_times_daily),
              before_n_stops = n(),
              before_avg_freq = mean(n_times_daily))

  after_stop_freq <- after_stop_freq |>  
    st_join(bgs) |>
    st_drop_geometry() |>
    group_by(GEOID, income_z, white_z, bach_z, priv_index) |>
    summarise(after_freq = sum(n_times_daily),
              after_n_stops = n(),
              after_avg_freq = mean(n_times_daily))
  
  this_comparison <- full_join(before_stop_freq, after_stop_freq) |>
    mutate(pct_change = (after_freq - before_freq) / before_freq,
           pct_change_n_stops = (after_n_stops - before_n_stops) / before_n_stops,
           pct_change_avg_freq = (after_avg_freq - before_avg_freq) / before_avg_freq) |>
    mutate(city = places[i]) |>
    select(city, income_z, white_z, bach_z, priv_index, 
           before_freq, after_freq, 
           pct_change, pct_change_avg_freq, pct_change_n_stops) |>
    filter(!is.na(income_z),
           !is.na(white_z),
           !is.na(bach_z),
           !is.na(pct_change))

  comparison <- rbind(comparison, this_comparison)
}

upper_bound <- quantile(comparison$pct_change, probs = 0.99)
comparison <- comparison |> filter(pct_change < upper_bound)
write_csv(comparison, here("Data", "bg_freqency.csv"))

### Summarize
fn_cor_est <- function(x, y) {
  cor.test(x, y)$estimate
}

fn_cor_p_val <- function(x, y) {
  cor.test(x, y)$p.value
}

fn_plot <- function(data, var, label) {
  ggplot(data,
         aes(x = !! sym(var),
             y = pct_change)) +
    geom_point(size = 0.1,
               shape = ".",
               alpha = 0.5,
               color = "lightblue") +
    stat_smooth(method = "lm",
                color = "black",
                fill = "black",
                linewidth = 0.1) +
    facet_wrap(~ city, ncol = 6) +
    geom_abline(slope = 0, intercept = 0,
                color = "gray",
                linetype = "dashed",
                linewidth = 0.1) +
    scale_x_continuous(name = label) +
    scale_y_continuous(name = "Percent change in transit service",
                       breaks = breaks <- seq(-1, 0.5, by = 0.5),
                       limits = c(-1, 0.5),
                       labels = paste0(breaks*100, "%")) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.1),
          strip.text = element_text(size = 12),
          panel.grid.major = element_line(linewidth = 0.2),
          panel.grid.minor = element_line(linewidth = 0.1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14))
}

summary_table <- comparison |>
  group_by(city) |>
  summarise(inc_cor = fn_cor_est(income_z, pct_change),
            inc_p = fn_cor_p_val(income_z, pct_change),
            race_cor = fn_cor_est(white_z, pct_change),
            race_p = fn_cor_p_val(white_z, pct_change),
            college_cor = fn_cor_est(bach_z, pct_change),
            college_p = fn_cor_p_val(bach_z, pct_change),
            priv_cor = fn_cor_est(priv_index, pct_change),
            priv_p = fn_cor_p_val(priv_index, pct_change))

# Overall privilege
priv_plot <- fn_plot(comparison, 
                     var = "priv_index",
                     "← Less privilege            More privilege →")

priv_plot
here("figures",
     "priv_plot.png") |> 
  ggsave(height = 3.5, width = 4, units = "in", dpi = 300)

# Income
inc_plot <- fn_plot(comparison, 
                     var = "income_z",
                     "← Lower income            Higher income →")

inc_plot
here("figures",
     "inc_plot.png") |> 
  ggsave(height = 3.5, width = 4, units = "in", dpi = 300)

# Race
race_plot <- fn_plot(comparison, 
                     var = "white_z",
                     paste0("← Lower proportion of white residents",
                            "            ",
                            "Higher proportion of white residents →"))

race_plot
here("figures",
     "race_plot.png") |> 
  ggsave(height = 3.5, width = 4, units = "in", dpi = 300)

# College
college_plot <- fn_plot(comparison, 
                     var = "bach_z",
                     paste0("← Lower proportion of residents with a bachelor degree",
                            "            ",
                            "Higher proportion of residents with a bachelor degree →"))

college_plot
here("figures",
     "college_plot.png") |> 
  ggsave(height = 3.5, width = 4, units = "in", dpi = 300)

write_csv(comparison, here("Data", "bg_freqency.csv"))
write_csv(summary_table, here("Data", "city_summary.csv"))
