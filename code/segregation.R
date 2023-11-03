library(tidyverse)
library(gtfstools)
library(tidytransit)
library(tigris)
library(tidycensus)
library(here)
library(ggthemes)
library(leaflet)
library(ggspatial)
library(ggmaptile)

options(tigris_use_cache = TRUE)

here("helper-functions",
     "gtfs-helpers.R") |>
  source()

cities <- get_decennial(geography = "place",
                        year = 2020,
                        variables = "P1_001N") |>
  filter(value > 300000) |>
  arrange(value)

dissimilarity_index <- function(minority_pops_by_area, 
                                total_pops_by_area) {
  
  minority_pops_by_area <- minority_pops_by_area[total_pops_by_area != 0]
  total_pops_by_area <- total_pops_by_area[total_pops_by_area != 0]
  
  proportions_by_area <- minority_pops_by_area / total_pops_by_area 
  
  total_minority_pop <- sum(minority_pops_by_area) # X
  overall_pop <- sum(total_pops_by_area) # T
  
  overall_prop <- total_minority_pop / overall_pop # P
  
  numerator <- sum(total_pops_by_area * abs(proportions_by_area - overall_prop))
  denominator <- (2*overall_pop*overall_prop)*(1 - overall_prop)
  dissimilarity <- numerator / denominator
  
  return(dissimilarity)
}

places <- here("combined-networks") |>
  list.dirs(recursive = FALSE,
            full.names = FALSE)

race_vars <- c(
  total_pop = "P1_001N",
  black_pop = "P2_006N",
  hispanic_pop = "P2_002N",
  asian_pop = "P2_008N",
  white_pop = "P2_005N")

zctas <- zctas(year = 2020) |>
  st_transform("WGS84") 

# summary <- tibble(City = "First",
#                   `Number of route areas` = 0,
#                   `Number of zip codes` = 0,
#                   `Black zip-code dissimilarity` = 0,
#                   `Black transit dissimilarity` = 0,
#                   `Asian zip-code dissimilarity` = 0,
#                   `Asian transit dissimilarity` = 0,
#                   `Hispanic zip-code dissimilarity` = 0,
#                   `Hispanic transit dissimilarity` = 0) |>
#   filter(City != "First")

summary <- here("Data", "segregation.csv") |>
  read_csv()

for(i in 1:length(places)) {
  
  if(!file.exists(here("combined-networks",
                      places[i],
                      "1Feb-segregation-data.csv"))) {
    stops <- here("combined-networks",
                        places[i],
                        "1Feb",
                        "GTFS.zip") |>
      read_gtfs() |>
      fn_get_stops_routes(as.POSIXct("2020-02-02"))
    
    state <- str_extract(places[i], '(?<=,\\s).+$') |>
      str_split("-") |>
      unlist()
    
    counties <- counties(state = state) |>
      st_transform("WGS84") |>
      st_filter(stops)
    
    
    blocks <- get_decennial(geography="block",
                            state = state, 
                            county = counties$COUNTYFP,
                            variables = race_vars,
                            year = 2020,
                            geometry = TRUE,
                            output = "wide") |>
      st_centroid() |>
      st_transform("WGS84") 
  
    blocks$n_stops <-  lengths(st_is_within_distance(blocks, stops, 
                                                 dist = 1000))
    blocks <- blocks |>
      filter(n_stops > 0)
    
    blocks <- st_join(blocks, stops, join = st_nearest_feature)
    
    blocks <- st_join(blocks, zctas) |>
      rename(zip_code = ZCTA5CE20) |>
      select(GEOID,
             zip_code,
             route_id,
             total_pop,
             black_pop,
             hispanic_pop,
             asian_pop,
             white_pop) |>
      st_drop_geometry()
    
    write_csv(blocks, here("combined-networks",
                           places[i],
                           "1Feb-segregation-data.csv"))
    
    zips <- blocks |>
      group_by(zip_code) |>
      summarise(total_pop = sum(total_pop),
                black_pop = sum(black_pop),
                hispanic_pop = sum(hispanic_pop),
                asian_pop = sum(asian_pop))
    
    routes <- blocks |>
      group_by(route_id) |>
      summarise(total_pop = sum(total_pop),
                black_pop = sum(black_pop),
                hispanic_pop = sum(hispanic_pop),
                asian_pop = sum(asian_pop))
  
    this_summary <- tibble(City = places[i],
                           `Number of route areas` = nrow(zips),
                           `Number of zip codes` = nrow(routes),
                           `Black zip-code dissimilarity` = 
                             dissimilarity_index(zips$black_pop, zips$total_pop),
                           `Black transit dissimilarity` = 
                             dissimilarity_index(routes$black_pop, routes$total_pop),
                           `Asian zip-code dissimilarity` = 
                             dissimilarity_index(zips$asian_pop, zips$total_pop),
                           `Asian transit dissimilarity` = 
                             dissimilarity_index(routes$asian_pop, routes$total_pop),
                           `Hispanic zip-code dissimilarity` = 
                             dissimilarity_index(zips$hispanic_pop, zips$total_pop),
                           `Hispanic transit dissimilarity` = 
                             dissimilarity_index(routes$hispanic_pop, routes$total_pop)) |>
      mutate(diff_count = (`Number of zip codes` - `Number of route areas`)/`Number of zip codes`)
    
    summary <- rbind(summary, this_summary)
  }
}

write_csv(summary, here("Data", "segregation.csv"))

blocks <- here("combined-networks",
               places[1],
               "1Feb-segregation-data.csv") |>
  read_csv() |>
  mutate(City = places[1])
  
for(i in 2:length(places)) {
  this_blocks <- here("combined-networks",
                 places[i],
                 "1Feb-segregation-data.csv") |>
    read_csv() |>
    mutate(City = places[i])
  
  blocks <- rbind(blocks, this_blocks)
}

zips <- blocks |>
  group_by(City, zip_code) |>
  summarise(total_pop = sum(total_pop)) |>
  filter(total_pop > 0) |>
  group_by(City) |>
  summarise(min_zip_pop = min(total_pop),
            pct_in_smallest_zip = min(total_pop) / sum(total_pop),
            pct_in_biggest_zip = max(total_pop) / sum(total_pop))

routes <- blocks |>
  group_by(City, route_id) |>
  summarise(total_pop = sum(total_pop)) |>
  filter(total_pop > 0) |>
  group_by(City) |>
  summarise(min_route_pop = min(total_pop),
            pct_in_smallest_route = min(total_pop) / sum(total_pop),
            pct_in_biggest_route = max(total_pop) / sum(total_pop))

summary_more <- left_join(summary, zips) |>
  left_join(routes) |>
  mutate(black_transit_better = 
           `Black transit dissimilarity` - `Black zip-code dissimilarity`) |>
  mutate(hisp_transit_better = 
           `Hispanic transit dissimilarity` - 
           `Hispanic zip-code dissimilarity`) |>
  mutate(asian_transit_better = 
           `Asian transit dissimilarity` - 
           `Asian zip-code dissimilarity`)



ggplot(summary) +
  geom_point(aes(x = `Black zip-code dissimilarity`,
                 y = `Black transit dissimilarity`),
             size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0,1),
                     name = "Zip-code-based dissimiliarity (Black residents)") +
  scale_y_continuous(limits = c(0,1),
                     name = "Transit-route-based dissimiliarity (Black residents)") +
  theme_minimal()

ggsave("black_seg.png",
       height = 4, width = 4, units = "in")

t.test(summary$`Black transit dissimilarity`,
       summary$`Black zip-code dissimilarity`,
       paired = TRUE)

ggplot(summary) +
  geom_point(aes(x = `Hispanic zip-code dissimilarity`,
                 y = `Hispanic transit dissimilarity`),
             size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0,1),
                     name = "Zip-code-based dissimiliarity (Hispanic residents)") +
  scale_y_continuous(limits = c(0,1),
                     name = "Transit-route-based dissimiliarity (Hispanic residents)") +
  theme_minimal()

ggsave("hisp_seg.png",
       height = 4, width = 4, units = "in")

t.test(summary$`Hispanic transit dissimilarity`,
       summary$`Hispanic zip-code dissimilarity`,
       paired = TRUE)

ggplot(summary) +
  geom_point(aes(x = `Asian zip-code dissimilarity`,
                 y = `Asian transit dissimilarity`),
             size = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(0,1),
                     name = "Zip-code-based dissimiliarity (Asian residents)") +
  scale_y_continuous(limits = c(0,1),
                     name = "Transit-route-based dissimiliarity (Asian residents)") +
  theme_minimal()

ggsave("asian_seg.png",
       height = 4, width = 4, units = "in")

t.test(summary$`Asian transit dissimilarity`,
       summary$`Asian zip-code dissimilarity`,
       paired = TRUE)

## MBTA example
points <- tibble(name = c("Contains point A",
                          "Contains point B",
                          "Contains point C",
                          "Contains point D",
                          "Contains point E",
                          "Contains point F",
                          "Contains point G",
                          "Contains point H",
                          "Contains point I",
                          "Contains point J"),
                 letter = c("A",
                          "B",
                          "C",
                          "D",
                          "E",
                          "F",
                          "G",
                          "H",
                          "I",
                          "J"),
                 lats = c(42.112509963059026,
                          42.479548800085524,
                          42.35867564829304,
                          42.3454408467407,
                          42.24692114384369, 
                          42.241167382549584, 
                          42.27595352949627, 
                          42.448382088500864,
                          42.519564887615644, 
                          42.39418995772039),
                 lons = c(-71.01333413865224,
                          -71.15238874431812,
                          -71.06033337929824,
                          -71.09725107427217,
                          -71.00742574702228,
                          -71.16478990244177,
                          -71.4163154554452,
                          -71.23035645569014,
                          -70.89821253160474,
                          -71.09725054707252)) |>
  st_as_sf(coords = c("lons", "lats"),
           crs = "WGS84")

MA_water <- area_water(state = "MA",
                      county = c("Barnstable",
                                 "Bristol", 
                                 "Worcester", 
                                 "Suffolk", 
                                 "Middlesex", 
                                 "Norfolk", 
                                 "Plymouth", 
                                 "Essex"))


  at(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  year = 2020,
  geometry = TRUE,
  cb = FALSE
) %>%
  st_transform(26918) %>%
  erase_water(year = 2020)

block_poly <- blocks(state = "MA", 
                        county = c("Barnstable",
                                   "Bristol", 
                                   "Worcester", 
                                   "Suffolk", 
                                   "Middlesex", 
                                   "Norfolk", 
                                   "Plymouth", 
                                   "Essex"))

Boston_blocks <- blocks |>
  filter(City == "Boston, MA") |>
  rename(GEOID20 = GEOID)

Boston_zips <- block_poly |>
  right_join(Boston_blocks) |>
  group_by(zip_code) %>% 
  summarize(geometry = st_union(geometry)) |>
  st_transform("WGS84") |>
  st_join(points) 

Boston_routes <- block_poly |>
  right_join(Boston_blocks) |>
  group_by(route_id) %>% 
  summarize(geometry = st_union(geometry)) |>
  st_transform("WGS84") |>
  st_join(points) 

bb <- st_bbox(Boston_routes)

zips_map <- ggplot(Boston_zips) +
  geom_sf(data = MA_water,
          color = NA,
          fill = "lightblue",
          alpha = 0.3) +
  # annotation_map_tile(type = "hillshade", zoomin = 0) +
  geom_sf(fill = NA, color = "gray") +
  geom_sf(data = Boston_zips[!is.na(Boston_zips$name),],
          aes(fill = name),
          alpha = 0.5) +
  geom_sf(data = points, aes(color = name)) +
  scale_fill_brewer(palette = "Set3", name = "") +
  scale_color_brewer(palette = "Set3", name = "") +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  theme_map() +
  theme(legend.position = "none")

zips_map

routes_map <- ggplot(Boston_routes) +
  geom_sf(data = MA_water,
          color = NA,
          fill = "lightblue",
          alpha = 0.3) +
  # annotation_map_tile(type = "hillshade", zoomin = 0) +
  geom_sf(fill = NA, color = "gray") +
  geom_sf(data = Boston_routes[!is.na(Boston_routes$name),],
          aes(fill = name),
          alpha = 0.5) +
  geom_sf(data = points, aes(color = name)) +
  scale_fill_brewer(palette = "Set3", name = "") +
  scale_color_brewer(palette = "Set3", name = "") +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  theme_map() +
  theme(legend.position = "none")

routes_map

both <- gridExtra::grid.arrange(zips_map, routes_map)

here("figures",
     "MBTA.png") |>
  ggsave(plot = both, width = 4, height = 6, units = "in")

leaflet(Boston_zips) |>
  addTiles() |>
  addPolygons(highlightOptions = highlightOptions(fillOpacity = 9),
              weight = 1) 

leaflet(Boston_routes) |>
  addTiles() |>
  addPolygons(highlightOptions = highlightOptions(fillOpacity = 1))
  
