########### 1 - HELPER FUNCTIONS ######################################################

library(tidyverse)
library(cancensus)
library(sf)
library(tmap)
library(tmaptools)
library(osmdata)

options(cancensus.api_key = "CensusMapper_7f389ba345baaf2f6be7df4037991aa5")
options(cancensus.cache_path = "~/OneDrive - McGill University/Documents/RA - UPGo/commercial-operators")

## Multilistings function

strr_multilistings <- function(daily, EH = 2, PR = 3, listing_type, host_ID,
                               date, cores){
  
  listing_type <- enquo(listing_type)
  host_ID <- enquo(host_ID)
  date <- enquo(date)
  
  daily %>%
    group_by(!! listing_type, !! host_ID, !! date)  %>%
    mutate(ML = ifelse(
      n() >= EH & !! listing_type == "Entire home/apt", TRUE,
      ifelse(n() >= PR & !! listing_type == "Private room", TRUE, FALSE))) %>%
    ungroup()
}
