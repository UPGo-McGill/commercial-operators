source("R/01-helper_functions.R")
source("R/02-data_import.R")

## Sample of all hosts with 20+ properties

commercial_hosts <- 
  commercial %>% 
  count(Airbnb_HID) %>% 
  filter(n >= 20)

write_csv(commercial_hosts, path = "data/commercial20_new.csv")


## Create random samples

FREHsample1 <- 
  FREH %>% 
  sample_n(1000)

write_csv(FREHsample1, path = "data/FREHsample1.csv")

commercialSample1 <- 
  commercial %>% 
  sample_n(1000)

write_csv(commercialSample1, path = "data/commercialSample1.csv")

