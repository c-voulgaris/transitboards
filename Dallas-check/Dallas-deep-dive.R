library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(tigris)

here("helper-functions",
     "gtfs-helpers.R") |>
  source()

bgs <- block_groups(state = "TX", county = c("113", "085", "121", "439")) |>
  # full_join(data) |>
  # filter(!is.na(city)) |>
  # select(before_freq, after_freq, pct_change) |>
  st_transform("WGS84")

before_gtfs <- here("Networks",
                         "Dallas, TX",
                         "1Feb",
                         "GTFS.zip") |>
  read_gtfs() 

after_gtfs <- here("Networks",
                          "Dallas, TX",
                          "31May",
                          "gtfs.zip") |>
  read_gtfs() 

before_first_day <- min(before_gtfs$calendar$start_date)
before_last_day <- max(before_gtfs$calendar$end_date)

after_first_day <- min(min(after_gtfs$calendar$start_date), 
                       min(after_gtfs$calendar_dates$date))
after_last_day <- max(max(after_gtfs$calendar$end_date), 
                       max(after_gtfs$calendar_dates$date))

before_n_days <- interval(before_first_day, before_last_day+1) / ddays(1)
after_n_days <- interval(after_first_day, after_last_day+1) / ddays(1)
  
  # For each service pattern, get the number of days it was effective
after_gtfs_exceptions_add <- after_gtfs$calendar_dates |>
  filter(exception_type == 1) |>
  group_by(service_id) |>
  summarise(added_days = n())

after_gtfs_exceptions_rm <- after_gtfs$calendar_dates |>
  filter(exception_type == 2) |>
  group_by(service_id) |>
  summarise(removed_days = n())


before_gtfs_service <- before_gtfs$calendar |>
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
           saturday * n_saturdays)|>
  select(service_id, n_service_days) |>
  mutate(pct_days = n_service_days / before_n_days)

after_gtfs_service <- after_gtfs$calendar |>
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
           saturday * n_saturdays)|>
  select(service_id, n_service_days) |>
  full_join(after_gtfs_exceptions_add) |>
  full_join(after_gtfs_exceptions_rm) |>
  replace_na(list(n_service_days = 0,
                  added_days = 0,
                  removed_days = 0)) |>
  mutate(n_service_days = n_service_days + added_days - removed_days) |>
  select(service_id, n_service_days) |>
    mutate(pct_days = n_service_days / after_n_days)
  
  my_gtfs_trip_freq <- my_gtfs$trips|>
    left_join(my_gtfs_service) |>
    select(trip_id, service_id, pct_days) |>
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


bg_pal <- colorNumeric(c("red", "yellow", "green"), bgs$pct_change)

map <- leaflet(bgs) |>
  addProviderTiles("Stamen.Toner.Light") |>
  addPolygons(weight = 1,
              color = ~bg_pal(pct_change),
              highlightOptions = highlightOptions(fillOpacity = 1),
              popup = ~htmlEscape(paste0(round(pct_change*100), "% change"))) |>
  addCircles(data = before_stop_freq, 
             radius = ~5*log(n_times_daily+1, base = 1.5),
             stroke = FALSE,
             fillOpacity = 0.8,
             group = "before",
             popup = ~htmlEscape(paste0(round(n_times_daily), " daily arrivals"))) |>
  addCircles(data = after_stop_freq, 
             radius = ~5*log(n_times_daily+1, base = 1.5),
             stroke = FALSE,
             fillOpacity = 0.8,
             group = "after",
             popup = ~htmlEscape(paste0(round(n_times_daily), " daily arrivals"))) |>
  addLayersControl(baseGroups = c("before", "after"),
                   options = layersControlOptions(collapsed = FALSE))
  
saveWidget(map, here("Dallas-check",
                     "map.html"))