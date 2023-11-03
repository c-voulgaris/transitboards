library(here)
library(tidyverse)
library(tidytransit)
library(lubridate)

before_gtfs <- here("Networks",
                    "Denver, CO",
                    "1Feb",
                    "GTFS.zip") |>
  read_gtfs()

after_gtfs <- here("Networks",
                   "Denver, CO",
                   "31May",
                   "GTFS.zip") |>
  read_gtfs()

before_week_of <- as.POSIXct("2020-02-02")
after_week_of <- as.POSIXct("2020-06-01")

# Check that feed is valid for the full week given
before_first_day <- min(before_gtfs$calendar$start_date)
before_last_day <- max(before_gtfs$calendar$end_date)

after_first_day <-  min(after_gtfs$calendar$start_date)
after_last_day <- max(after_gtfs$calendar$end_date)

before_gtfs_service <- before_gtfs$calendar |>
  filter(start_date <= before_week_of,
         end_date >= (before_week_of + ddays(7))) |>
  mutate(n_service_days = 
           sunday +
           monday +
           tuesday +
           wednesday +
           thursday +
           friday +
           saturday) |>
  select(service_id, n_service_days) |>
  mutate(pct_days = n_service_days / 7)

after_gtfs_service <- after_gtfs$calendar |>
  filter(start_date <= after_week_of,
         end_date >= (after_week_of + ddays(7))) |>
  mutate(n_service_days = 
           sunday +
           monday +
           tuesday +
           wednesday +
           thursday +
           friday +
           saturday) |>
  select(service_id, n_service_days) |>
  mutate(pct_days = n_service_days / 7)



