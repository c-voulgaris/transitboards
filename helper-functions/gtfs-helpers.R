library(tidyverse)
library(lubridate)
library(sf)

fn_daily_stop_freq <- function(my_gtfs) {
  
  if(length(my_gtfs$calendar_dates$date) > 0 &
     length(my_gtfs$calendar$start_date) > 0) {
    first_day <- min(min(my_gtfs$calendar$start_date), 
                     min(my_gtfs$calendar_dates$date))
    
    last_day <- max(max(my_gtfs$calendar$end_date),
                    max(my_gtfs$calendar_dates$date))
    
  } else if (length(my_gtfs$calendar$start_date) > 0){
    first_day <- min(my_gtfs$calendar$start_date)
    last_day <- max(my_gtfs$calendar$end_date)
  } else {
    first_day <- min(my_gtfs$calendar_dates$date)
    last_day <- max(my_gtfs$calendar_dates$date)
  }
  
  n_days <- interval(first_day, last_day+1) / ddays(1)
  
  # For each service pattern, get the number of days it was effective
  if(length(my_gtfs$calendar_dates$date) > 0) {
    my_gtfs_exceptions_add <- my_gtfs$calendar_dates |>
      filter(exception_type == 1) |>
      group_by(service_id) |>
      summarise(added_days = n())
    
    my_gtfs_exceptions_rm <- my_gtfs$calendar_dates |>
      filter(exception_type == 2) |>
      group_by(service_id) |>
      summarise(removed_days = n())
  }
  
  if(length(my_gtfs$calendar$start_date) > 0) {
    my_gtfs_service <- my_gtfs$calendar |>
      mutate(n_weeks_full = interval(start_date, end_date+1) / dweeks(1)) |>
      mutate(n_weeks_whole = floor(n_weeks_full)) |>
      mutate(xtra_days = (n_weeks_full - n_weeks_whole) * 7) |>
      mutate(n_sundays = ifelse(xtra_days > 0, n_weeks_whole +1, n_weeks_whole),
             n_mondays = ifelse(xtra_days > 1, n_weeks_whole +1, n_weeks_whole),
             n_tuesdays = ifelse(xtra_days > 2, n_weeks_whole +1, n_weeks_whole),
             n_wednesdays = ifelse(xtra_days > 3, n_weeks_whole +1, n_weeks_whole),
             n_thursdays = ifelse(xtra_days > 4, n_weeks_whole +1, n_weeks_whole),
             n_fridays = ifelse(xtra_days > 1, n_weeks_whole +1, n_weeks_whole),
             n_saturdays = ifelse(xtra_days > 1, n_weeks_whole +1, n_weeks_whole)) |>
      mutate(n_service_days = 
               sunday * n_sundays +
               monday * n_mondays +
               tuesday * n_tuesdays +
               wednesday * n_wednesdays +
               thursday * n_thursdays +
               friday * n_fridays +
               saturday * n_saturdays) |>
      select(service_id, n_service_days)
    
    if(length(my_gtfs$calendar_dates$date) > 0) {
      my_gtfs_service <- my_gtfs_service |>
        full_join(my_gtfs_exceptions_add) |>
        full_join(my_gtfs_exceptions_rm) |>
        replace_na(list(n_service_days = 0,
                        added_days = 0,
                        removed_days = 0)) |>
        mutate(n_service_days = n_service_days + added_days - removed_days) |>
        select(service_id, n_service_days)
    }
  } else {
    my_gtfs_service <- my_gtfs_exceptions_add |>
      rename(n_service_days = added_days)
  }
  
  my_gtfs_stop_times <- my_gtfs$trips|>
    left_join(my_gtfs_service) |>
    select(trip_id, service_id, n_service_days) |>
    left_join(my_gtfs$stop_times) |>
    select(stop_id, trip_id, service_id, n_service_days) |>
    group_by(stop_id) |>
    summarise(n_times_daily = sum(n_service_days)/n_days) |>
    filter(!is.na(stop_id)) |>
    left_join(my_gtfs$stops) |>
    select(stop_id, stop_lat, stop_lon, n_times_daily) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = "WGS84")
  
}