### 1 - MONTREAL ANALYSIS LIBRARIES AND HELPER FUNCTIONS ######################################

library(tidyverse)
library(upgo)
library(strr)
library(future)
library(sf)
library(zoo)
library(ggplot2)
library(cancensus)
library(osmdata)
library(extrafont)
library(cowplot)
plan(multiprocess)

options(cancensus.api_key = "CensusMapper_7f389ba345baaf2f6be7df4037991aa5")
options(cancensus.cache_path = "C:/Users/charl/OneDrive - McGill University/Documents/630-633 - SRP + SSHRC/Data/census")