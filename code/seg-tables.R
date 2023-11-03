library(here)
library(tidyverse)

places <- here("combined-networks") |>
  list.dirs(recursive = FALSE,
            full.names = FALSE)

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