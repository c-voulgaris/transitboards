library(tidytransit)
library(tidyverse)
library(here)
library(hms)

here("helper-functions",
     "gtfs-helpers.R") |>
  source()

example <- here("Networks",
                "Boston, MA",
                "31May",
                "GTFS.zip") |>
  read_gtfs()

agency <- tibble(agency_id = "1",
                 agency_name = "Test",
                 agency_url = "https://urban-stack.github.io/",
                 agency_timezone = "America/New_York",
                 agency_lang = "EN",
                 agency_phone = "617-496-2602")

# Define weekday, Saturday, and Sunday service
calendar <- tibble(service_id = c("Weekday","Saturday","Sunday"),
                   monday = c(1, 0, 0),
                   tuesday = c(1, 0, 0),
                   wednesday = c(1, 0, 0),
                   thursday = c(1, 0, 0),
                   friday = c(1, 0, 0),
                   saturday = c(0, 1, 0),
                   sunday = c(0, 0, 1),
                   start_date = rep(as.Date("2023-05-01"),3),
                   end_date = rep(as.Date("2023-05-28"),3))

# Add special birthday route on May 8, 
# Change May 14 (a Sunday) to weekday service.
# Change May 17 (a Wednesday) to Saturday service.
calendar_dates <- tibble(service_id = c("Weekday",
                                        "Weekday",
                                        "Saturday",
                                        "Sunday",
                                        "Birthday"),
                         date = c(as.Date("2023-05-14"),
                                  as.Date("2023-05-17"),
                                  as.Date("2023-05-17"),
                                  as.Date("2023-05-14"),
                                  as.Date("2023-05-08")),
                         exception_type = c(1,
                                            2,
                                            1,
                                            2,
                                            1))

stops <- tibble(stop_id = c("home", "work"),
                stop_lat = c(42.394506,
                             42.376635),
                stop_lon = c(-71.096887,
                             -71.113982),
                stop_name = c("My home",
                              "My work"))

first_times <- seq(8*3600, 18*3600, by = 15*60)
times <- c(new_hms(first_times),
           new_hms(first_times + 5*60),
           new_hms(first_times + 6*60),
           new_hms(first_times + 11*60))

stop_times <- tibble(trip_id = c(rep(paste0(seq(1, 41, by=1), "s"), 2),
                                 rep(paste0(seq(1, 41, by=1), "n"), 2)),
                     arrival_time = times,
                     departure_time = times,
                     stop_id = c(rep("home", 41), rep("work", 41),
                                 rep("work", 41), rep("home", 41)),
                     stop_sequence = c(rep(1, 41), rep(2, 41),
                                       rep(1, 41), rep(2, 41)))

trips <- tibble(route_id = c(rep("s", 41),
                             rep("n", 41),
                             rep("s", 21),
                             rep("n", 21),
                             rep("s", 11),
                             rep("n", 11),
                             "s"),
                service_id = c(rep("Weekday", 82),
                               rep("Saturday", 42),
                               rep("Sunday", 22),
                               "Birthday"),
                trip_id = c(paste0(seq(1, 41, by=1), "s"),
                            paste0(seq(1, 41, by=1), "n"),
                            paste0(seq(1, 41, by=2), "s"),
                            paste0(seq(1, 41, by=2), "n"),
                            paste0(seq(1, 41, by=4), "s"),
                            paste0(seq(1, 41, by=4), "n"),
                            "17s"))

routes <- tibble(route_id = c("s", "n"),
                 route_long_name = c("Home to work",
                                     "Work to home"),
                 route_short_name = c("h2w", "w2h"),
                 route_type = c(6, 6))

                                      
test_gtfs <- example[c(1, 2, 4, 17, 19, 20, 22)]
test_gtfs$agency <- agency
test_gtfs$calendar <- calendar
test_gtfs$calendar_dates <- calendar_dates
test_gtfs$routes <- routes
test_gtfs$stop_times <- stop_times
test_gtfs$stops <- stops
test_gtfs$trips <- trips

validate <- validate_gtfs(test_gtfs)

test_freq_stop <- fn_daily_stop_freq(test_gtfs)

bgs <- tigris::block_groups(state = "MA", county = "Middlesex") |>
  st_transform("WGS84")

test_freq_trip <- fn_daily_trip_freq(test_gtfs, bgs)
