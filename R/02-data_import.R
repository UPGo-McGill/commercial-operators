#### 2 - MONTREAL ANALYSIS DATA IMPORT #########################################################

source("R/01-helper_functions.R")

### Import data ################################################################

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Montreal") %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect()

host <- 
  host_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

upgo_disconnect()


### Prepare data ###############################################################

daily <- 
  daily %>% 
  strr_expand()

host <- 
  host %>% 
  strr_expand()

daily <- 
  daily %>% 
  strr_multi(host)

FREH <- 
  daily %>% 
  strr_FREH("2015-01-01", "2019-12-31")

GH <- 
  property %>% 
  strr_as_sf(32618) %>% 
  strr_ghost("2015-01-01", "2019-12-31")

save(property, daily, host, FREH, GH, file = "Montreal_data.Rdata")
