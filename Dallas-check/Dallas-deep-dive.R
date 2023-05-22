library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

here("helper-functions",
     "gtfs-helpers.R") |>
  source()

data <- here("Data",
             "bg_freqency.csv") |>
  read_csv() |>
  filter(city == "Dallas, TX")

bgs <- block_groups(state = "TX", county = c("113", "085", "121", "439")) |>
  full_join(data) |>
  filter(!is.na(city)) |>
  select(before_freq, after_freq, pct_change) |>
  st_transform("WGS84")

before_stop_freq <- here("Networks",
                         "Dallas, TX",
                         "1Feb",
                         "GTFS.zip") |>
  read_gtfs() |>
  fn_daily_stop_freq() 

after_stop_freq <- here("Networks",
                          "Dallas, TX",
                          "31May",
                          "gtfs.zip") |>
  read_gtfs() |>
  fn_daily_stop_freq() 


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