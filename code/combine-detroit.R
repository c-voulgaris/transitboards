library(here)
library(tidyverse)
library(gtfstools)

places <- here("Networks") |>
  list.dirs(recursive = FALSE,
            full.names = FALSE)

for(i in 1:length(places)) {
  
  dir.create(here("combined-networks",
                  places[i]))
  
  dir.create(here("combined-networks",
                  places[i],
                  "1Feb"))
  
  dir.create(here("combined-networks",
                  places[i],
                  "31May"))
  
  before_feeds <- here("Networks",
                       places[i],
                       "1Feb") |>
    list.files(recursive = FALSE,
               full.names = FALSE)
  
  after_feeds <- here("Networks",
                      places[i],
                      "31May") |>
    list.files(recursive = FALSE,
               full.names = FALSE)
  
  if(length(before_feeds) == 1) {
    file.copy(from = here("Networks",
                          places[i],
                          "1Feb",
                          before_feeds[1]), 
              to = here("combined-networks",
                        places[i],
                        "1Feb",
                        "GTFS.zip"))
  } else {
    gtfs_1 <- read_gtfs(here("Networks",
                             places[i],
                             "1Feb",
                             before_feeds[1]))
    gtfs_2 <- read_gtfs(here("Networks",
                             places[i],
                             "1Feb",
                             before_feeds[2]))
    
    gtfs_list <- list(gtfs_1, gtfs_2)
    
    if(length(before_feeds) > 2) {
      for(j in 3:length(before_feeds)) {
        gtfs_next <- read_gtfs(here("Networks",
                                    places[i],
                                    "1Feb",
                                    before_feeds[j]))
        
        gtfs_list[[j]] <- gtfs_next
      }
    }
    gtfs_merged <- merge_gtfs(gtfs_list)
    write_gtfs(gtfs_merged, here("combined-networks",
                                 places[i],
                                 "1Feb",
                                 "GTFS.zip"))
  }
  if(length(after_feeds) == 1) {
    file.copy(from = here("Networks",
                          places[i],
                          "31May",
                          after_feeds[1]), 
              to = here("combined-networks",
                        places[i],
                        "31May",
                        "GTFS.zip"))
  } else {
    gtfs_1 <- read_gtfs(here("Networks",
                             places[i],
                             "31May",
                             after_feeds[1]))
    gtfs_2 <- read_gtfs(here("Networks",
                             places[i],
                             "31May",
                             after_feeds[2]))
    
    gtfs_list <- list(gtfs_1, gtfs_2)
    
    if(length(after_feeds) > 2) {
      for(j in 3:length(after_feeds)) {
        gtfs_next <- read_gtfs(here("Networks",
                                    places[i],
                                    "31May",
                                    after_feeds[j]))
        
        gtfs_list[[j]] <- gtfs_next
      }
    }
    gtfs_merged <- merge_gtfs(gtfs_list)
    write_gtfs(gtfs_merged, here("combined-networks",
                                 places[i],
                                 "31May",
                                 "GTFS.zip"))
  }
}
