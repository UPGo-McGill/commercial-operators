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
host <- strr_expand(host) |> as_tibble()
daily <- strr_multi(daily, host)
FREH <- strr_FREH(daily, "2015-01-01", "2019-12-31")
GH <- strr_ghost(strr_as_sf(property, 32618), "2015-01-01", "2019-12-31")


# Get census data ---------------------------------------------------------

province <- 
  get_census("CA16", regions = list(PR = "24"), geo_format = "sf") |> 
  st_transform(32618) |> 
  select(geometry)

CT <- cancensus::get_census(dataset = "CA16", 
                            regions = list(CSD = c("2466023")), 
                            level = "CT", geo_format = "sf") |> 
  st_transform(32618)


# Join STR data to census -------------------------------------------------

property <- 
  property |> 
  strr_as_sf(32618) |> 
  strr_raffle(CT, GeoUID, units = Dwellings)

property <- 
  property |> 
  select(-.grid_ID)

qsavem(property, daily, host, FREH, GH, CT, province, file = "output/data.qsm",
       nthreads = availableCores())
