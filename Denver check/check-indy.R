library(here)
library(tidyverse)
library(tidytransit)
library(lubridate)

before_gtfs <- here("Networks",
                    "Indianapolis, IN",
                    "1Feb",
                    "gtfs.zip") |>
  read_gtfs()

after_gtfs <- here("Networks",
                   "Indianapolis, IN",
                   "31May",
                   "gtfs.zip") |>
  read_gtfs()

before_week_of <- as.POSIXct("2020-02-02")
after_week_of <- as.POSIXct("2020-06-01")

# Check that feed is valid for the full week given
before_first_day <- min(before_gtfs$calendar_dates$date)
before_last_day <- max(before_gtfs$calendar_dates$date)

after_first_day <-  min(after_gtfs$calendar_dates$date)
after_last_day <- max(after_gtfs$calendar_dates$date)

before_gtfs_exceptions_add <- before_gtfs$calendar_dates |>
  filter(exception_type == 1,
         date >= before_week_of,
         date <= (before_week_of + ddays(6)))  |>
  group_by(service_id) |>
  summarise(added_days = n())

before_gtfs_exceptions_rm <- before_gtfs$calendar_dates |>
  filter(exception_type == 2,
         date >= before_week_of,
         date <= (before_week_of + ddays(6)))  |>
  group_by(service_id) |>
  summarise(removed_days = n())

after_gtfs_exceptions_add <- after_gtfs$calendar_dates |>
  filter(exception_type == 1,
         date >= after_week_of,
         date <= (after_week_of + ddays(6)))  |>
  group_by(service_id) |>
  summarise(added_days = n())

after_gtfs_exceptions_rm <- after_gtfs$calendar_dates |>
  filter(exception_type == 2,
         date >= after_week_of,
         date <= (after_week_of + ddays(7)))  |>
  group_by(service_id) |>
  summarise(removed_days = n())

