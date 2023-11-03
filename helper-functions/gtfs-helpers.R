library(tidyverse)
library(lubridate)
library(sf)

fn_daily_trip_freq <- function(my_gtfs, 
                               my_bgs,
                               week_of) {
  
  # Check that feed is valid for the full week given
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
  
  ## If the feed isn't valid for the full week, 
  ## shift by 2 days each direction before giving up.
  if (first_day > week_of |
      last_day < (week_of + ddays(6))) {
    
    week_of <- week_of + ddays(1) # one day later than orig.
    
    if (first_day > week_of |
        last_day < (week_of + ddays(6))) {
      
      week_of <- week_of - ddays(2) # one day earlier than orig.
      
      if (first_day > week_of |
          last_day < (week_of + ddays(6))) {
        
        week_of <- week_of + ddays(3) # two days later than orig
        if (first_day > week_of |
            last_day < (week_of + ddays(6))) {
          
          week_of <- week_of - ddays(4) # two days before orig
        
          if (first_day > week_of |
              last_day < (week_of + ddays(6))) {
            
            stop(paste0(my_gtfs$agency$agency_name,
                        " Feed not valid for weeks beginning ",
                        week_of,
                        " through ",
                        week_of + ddays(4)))
          }
        }
      }
    }
      
  }
  
  # For each service pattern, get the number of days it was effective
  if(length(my_gtfs$calendar_dates$date) > 0) {
    my_gtfs_exceptions_add <- my_gtfs$calendar_dates |>
      filter(exception_type == 1,
             date >= week_of,
             date <= (week_of + ddays(6)))  |>
      group_by(service_id) |>
      summarise(added_days = n())
    
    my_gtfs_exceptions_rm <- my_gtfs$calendar_dates |>
      filter(exception_type == 2,
             date >= week_of,
             date <= (week_of + ddays(6)))  |>
      group_by(service_id) |>
      summarise(removed_days = n())
  }
  
  if (weekdays(week_of) == "Sunday") {
    week_of_sun <- week_of
    week_of_mon <- week_of + ddays(1)
    week_of_tue <- week_of + ddays(2)
    week_of_wed <- week_of + ddays(3)
    week_of_thu <- week_of + ddays(4)
    week_of_fri <- week_of + ddays(5)
    week_of_sat <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Monday") {
    week_of_mon <- week_of
    week_of_tue <- week_of + ddays(1)
    week_of_wed <- week_of + ddays(2)
    week_of_thu <- week_of + ddays(3)
    week_of_fri <- week_of + ddays(4)
    week_of_sat <- week_of + ddays(5)
    week_of_sun <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Tuesday") {
    week_of_tue <- week_of
    week_of_wed <- week_of + ddays(1)
    week_of_thu <- week_of + ddays(2)
    week_of_fri <- week_of + ddays(3)
    week_of_sat <- week_of + ddays(4)
    week_of_sun <- week_of + ddays(5)
    week_of_mon <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Wednesday") {
    week_of_wed <- week_of
    week_of_thu <- week_of + ddays(1)
    week_of_fri <- week_of + ddays(2)
    week_of_sat <- week_of + ddays(3)
    week_of_sun <- week_of + ddays(4)
    week_of_mon <- week_of + ddays(5)
    week_of_tue <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Thursday") {
    week_of_thu <- week_of
    week_of_fri <- week_of + ddays(1)
    week_of_sat <- week_of + ddays(2)
    week_of_sun <- week_of + ddays(3)
    week_of_mon <- week_of + ddays(4)
    week_of_tue <- week_of + ddays(5)
    week_of_wed <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Friday") {
    week_of_fri <- week_of
    week_of_sat <- week_of + ddays(1)
    week_of_sun <- week_of + ddays(2)
    week_of_mon <- week_of + ddays(3)
    week_of_tue <- week_of + ddays(4)
    week_of_wed <- week_of + ddays(5)
    week_of_thu <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Saturday") {
    week_of_sat <- week_of
    week_of_sun <- week_of + ddays(1)
    week_of_mon <- week_of + ddays(2)
    week_of_tue <- week_of + ddays(3)
    week_of_wed <- week_of + ddays(4)
    week_of_thu <- week_of + ddays(5)
    week_of_fri <- week_of + ddays(6)
  }
  
  if(length(my_gtfs$calendar$start_date) > 0) {
    my_gtfs_service <- my_gtfs$calendar |>
      mutate(sunday = ifelse(start_date <= week_of_sun & 
                               end_date >= week_of_sun, 
                             sunday, 0)) |>
      mutate(monday = ifelse(start_date <= week_of_mon & 
                               end_date >= week_of_mon, 
                             monday, 0)) |>
      mutate(tuesday = ifelse(start_date <= week_of_tue & 
                               end_date >= week_of_tue, 
                             tuesday, 0)) |>
      mutate(wednesday = ifelse(start_date <= week_of_wed & 
                               end_date >= week_of_wed, 
                             wednesday, 0)) |>
      mutate(thursday = ifelse(start_date <= week_of_thu & 
                               end_date >= week_of_thu, 
                             thursday, 0)) |>
      mutate(friday = ifelse(start_date <= week_of_fri & 
                               end_date >= week_of_fri, 
                             friday, 0)) |>
      mutate(saturday = ifelse(start_date <= week_of_sat & 
                               end_date >= week_of_sat, 
                             saturday, 0)) |>
      mutate(n_service_days = 
               sunday +
               monday +
               tuesday +
               wednesday +
               thursday +
               friday +
               saturday) |>
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
  
  my_gtfs_service <- my_gtfs_service |>
    mutate(pct_days = n_service_days / 7) |>
    filter(pct_days > 0)
  
  my_gtfs_trip_freq <- my_gtfs$trips |>
    left_join(my_gtfs_service) |>
    select(trip_id, service_id, pct_days) |>
    filter(!is.na(pct_days)) |>
    left_join(my_gtfs$stop_times) |>
    select(stop_id, trip_id, service_id, pct_days) |>
    filter(!is.na(stop_id)) |>
    left_join(my_gtfs$stops) |>
    #select(stop_id, stop_lat, stop_lon, n_times_daily) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = "WGS84")|>
    st_join(my_bgs) |>
    st_drop_geometry()|>
    group_by(GEOID, service_id) |>
    mutate(n_trips = length(unique(trip_id)) * pct_days) |>
    summarise(n_trips = mean(n_trips)) |>
    ungroup() |>
    group_by(GEOID) |>
    summarise(n_trips = sum(n_trips))

}

fn_get_stops_routes <- function(my_gtfs, 
                                week_of) {
  
  # Get the weekly frequency of each route
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
  
  ## If the feed isn't valid for the full week, 
  ## shift by 2 days each direction before giving up.
  if (first_day > week_of |
      last_day < (week_of + ddays(6))) {
    
    week_of <- week_of + ddays(1) # one day later than orig.
    
    if (first_day > week_of |
        last_day < (week_of + ddays(6))) {
      
      week_of <- week_of - ddays(2) # one day earlier than orig.
      
      if (first_day > week_of |
          last_day < (week_of + ddays(6))) {
        
        week_of <- week_of + ddays(3) # two days later than orig
        if (first_day > week_of |
            last_day < (week_of + ddays(6))) {
          
          week_of <- week_of - ddays(4) # two days before orig
          
          if (first_day > week_of |
              last_day < (week_of + ddays(6))) {
            
            stop(paste0(my_gtfs$agency$agency_name,
                        " Feed not valid for weeks beginning ",
                        week_of,
                        " through ",
                        week_of + ddays(4)))
          }
        }
      }
    }
    
  }
  
  # For each service pattern, get the number of days it was effective
  if(length(my_gtfs$calendar_dates$date) > 0) {
    my_gtfs_exceptions_add <- my_gtfs$calendar_dates |>
      filter(exception_type == 1,
             date >= week_of,
             date <= (week_of + ddays(6)))  |>
      group_by(service_id) |>
      summarise(added_days = n())
    
    my_gtfs_exceptions_rm <- my_gtfs$calendar_dates |>
      filter(exception_type == 2,
             date >= week_of,
             date <= (week_of + ddays(6)))  |>
      group_by(service_id) |>
      summarise(removed_days = n())
  }
  
  if (weekdays(week_of) == "Sunday") {
    week_of_sun <- week_of
    week_of_mon <- week_of + ddays(1)
    week_of_tue <- week_of + ddays(2)
    week_of_wed <- week_of + ddays(3)
    week_of_thu <- week_of + ddays(4)
    week_of_fri <- week_of + ddays(5)
    week_of_sat <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Monday") {
    week_of_mon <- week_of
    week_of_tue <- week_of + ddays(1)
    week_of_wed <- week_of + ddays(2)
    week_of_thu <- week_of + ddays(3)
    week_of_fri <- week_of + ddays(4)
    week_of_sat <- week_of + ddays(5)
    week_of_sun <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Tuesday") {
    week_of_tue <- week_of
    week_of_wed <- week_of + ddays(1)
    week_of_thu <- week_of + ddays(2)
    week_of_fri <- week_of + ddays(3)
    week_of_sat <- week_of + ddays(4)
    week_of_sun <- week_of + ddays(5)
    week_of_mon <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Wednesday") {
    week_of_wed <- week_of
    week_of_thu <- week_of + ddays(1)
    week_of_fri <- week_of + ddays(2)
    week_of_sat <- week_of + ddays(3)
    week_of_sun <- week_of + ddays(4)
    week_of_mon <- week_of + ddays(5)
    week_of_tue <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Thursday") {
    week_of_thu <- week_of
    week_of_fri <- week_of + ddays(1)
    week_of_sat <- week_of + ddays(2)
    week_of_sun <- week_of + ddays(3)
    week_of_mon <- week_of + ddays(4)
    week_of_tue <- week_of + ddays(5)
    week_of_wed <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Friday") {
    week_of_fri <- week_of
    week_of_sat <- week_of + ddays(1)
    week_of_sun <- week_of + ddays(2)
    week_of_mon <- week_of + ddays(3)
    week_of_tue <- week_of + ddays(4)
    week_of_wed <- week_of + ddays(5)
    week_of_thu <- week_of + ddays(6)
  }
  if (weekdays(week_of) == "Saturday") {
    week_of_sat <- week_of
    week_of_sun <- week_of + ddays(1)
    week_of_mon <- week_of + ddays(2)
    week_of_tue <- week_of + ddays(3)
    week_of_wed <- week_of + ddays(4)
    week_of_thu <- week_of + ddays(5)
    week_of_fri <- week_of + ddays(6)
  }
  
  if(length(my_gtfs$calendar$start_date) > 0) {
    my_gtfs_service <- my_gtfs$calendar |>
      mutate(sunday = ifelse(start_date <= week_of_sun & 
                               end_date >= week_of_sun, 
                             sunday, 0)) |>
      mutate(monday = ifelse(start_date <= week_of_mon & 
                               end_date >= week_of_mon, 
                             monday, 0)) |>
      mutate(tuesday = ifelse(start_date <= week_of_tue & 
                                end_date >= week_of_tue, 
                              tuesday, 0)) |>
      mutate(wednesday = ifelse(start_date <= week_of_wed & 
                                  end_date >= week_of_wed, 
                                wednesday, 0)) |>
      mutate(thursday = ifelse(start_date <= week_of_thu & 
                                 end_date >= week_of_thu, 
                               thursday, 0)) |>
      mutate(friday = ifelse(start_date <= week_of_fri & 
                               end_date >= week_of_fri, 
                             friday, 0)) |>
      mutate(saturday = ifelse(start_date <= week_of_sat & 
                                 end_date >= week_of_sat, 
                               saturday, 0)) |>
      mutate(n_service_days = 
               sunday +
               monday +
               tuesday +
               wednesday +
               thursday +
               friday +
               saturday) |>
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
  
  my_gtfs_service <- my_gtfs_service |>
    mutate(pct_days = n_service_days / 7) |>
    filter(pct_days > 0)
  
  my_gtfs_routes <- my_gtfs$trips |>
    inner_join(my_gtfs_service) |>
    group_by(service_id, route_id, pct_days) |>
    summarise(n_trips = n()) |>
    mutate(weekly_trips = n_trips * pct_days) |>
    ungroup() |>
    group_by(route_id) |>
    summarise(weekly_trips = sum(weekly_trips))
    
  my_gtfs_trips <- my_gtfs$trips |>
    select(trip_id, route_id)
    
  my_gtfs_stops <- my_gtfs$stop_times |>
    left_join(my_gtfs_trips) |>
    group_by(stop_id, route_id) |>
    summarise(n = n()) |>
    left_join(my_gtfs_routes) |>
    group_by(stop_id) |>
    mutate(rank = dense_rank(weekly_trips),
           num = n()) |>
    filter(rank == num) |>
    select(stop_id, route_id) |>
    left_join(my_gtfs$stops) |>
    select(stop_id, route_id, stop_lat, stop_lon) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"),
             crs = "WGS84")
  
  my_gtfs_stops
}
 