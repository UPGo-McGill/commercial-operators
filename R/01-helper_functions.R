########### 1 - HELPER FUNCTIONS ######################################################

library(tidyverse)
library(cancensus)
library(sf)
library(tmap)
library(tmaptools)
library(osmdata)

options(cancensus.api_key = "CensusMapper_7f389ba345baaf2f6be7df4037991aa5")
options(cancensus.cache_path = "~/OneDrive - McGill University/Documents/RA - UPGo/commercial-operators")
