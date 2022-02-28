#### DATA IMPORT ###############################################################

source("R/01_startup.R")


# Import data -------------------------------------------------------------

upgo_connect()

property <- 
  property_remote |> 
  filter(country == "Canada", city == "Montreal") |> 
  collect()

daily <- 
  daily_remote |> 
  filter(property_ID %in% !!property$property_ID, start_date <= "2019-12-31") |> 
  collect()

host <- 
  host_remote |> 
  filter(host_ID %in% !!property$host_ID, start_date <= "2019-12-31") |> 
  collect()

upgo_disconnect()


# Prepare data ------------------------------------------------------------

daily <- strr_expand(daily)
host <- strr_expand(host)
daily <- strr_multi(daily, host)
FREH <- strr_FREH(daily, "2015-01-01", "2019-12-31")
GH <- strr_ghost(strr_as_sf(property, 32618), "2015-01-01", "2019-12-31")

qsavem(property, daily, host, FREH, GH, file = "output/data.qsm",
       nthreads = availableCores())
