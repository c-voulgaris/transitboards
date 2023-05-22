library(tidycensus)
library(tidyverse)

msas <- get_decennial(geography = "cbsa", variables = c("P1_001N"), year = 2020) |>
  arrange(desc(value))