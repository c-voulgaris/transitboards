library(tidytransit)
library(tidyverse)

filter_agency <- function(gtfs_feed,
                          url) {
  
  gtfs_feed$agency <- gtfs_feed$agency |>
    filter(agency_url == url)
  
  gtfs_feed$routes <- gtfs_feed$routes |>
    filter(agency_id %in% gtfs_feed$agency$agency_id)
  
  gtfs_feed$trips <- gtfs_feed$trips |>
    filter(route_id %in% gtfs_feed$routes$route_id)
  
  gtfs_feed$calendar <- gtfs_feed$calendar |>
    filter(service_id %in% gtfs_feed$trips$service_id)
  
  gtfs_feed$calendar_dates <- gtfs_feed$calendar_dates |>
    filter(service_id %in% gtfs_feed$trips$service_id)
  
  gtfs_feed$stop_times <- gtfs_feed$stop_times |>
    filter(trip_id %in% gtfs_feed$trips$trip_id)
  
  gtfs_feed$stops <- gtfs_feed$stops |>
    filter(stop_id %in% gtfs_feed$stop_times$stop_id)
  
  gtfs_feed
}

here("Networks",
     "Minneapolis, MN",
     "combined",
     "1Feb.zip") |>
  read_gtfs() |>
  filter_agency("https://www.metrotransit.org") |>
  write_gtfs(here("Networks",
                  "Minneapolis, MN",
                  "1Feb",
                  "gtfs.zip"))

here("Networks",
     "Minneapolis, MN",
     "combined",
     "31May.zip") |>
  read_gtfs() |>
  filter_agency("https://www.metrotransit.org") |>
  write_gtfs(here("Networks",
                  "Minneapolis, MN",
                  "31May",
                  "gtfs.zip"))
