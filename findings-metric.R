## For each block group in the service area:

# Percent change in transit frequency
# Median income
# Percent non-white
# Percent with no bachelor's degree

library(tidyverse)
library(sf)
library(tidytransit)
library(here)
library(lubridate)

# Los Angeles
la_before <- here("Networks",
     "LA",
     "1Feb",
     "GTFS.zip") |>
  read_gtfs()

# For each service pattern, get the number of days it was effective
la_before_calendar <- la_before$calendar |>
  mutate(n_days = end_date - start_date +1) |>
  mutate(n_weeks_full = interval(start_date, end_date+1) / dweeks(1)) |>
  mutate(xtra_days = )

         