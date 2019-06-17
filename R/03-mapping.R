########### 3 - MAPPING ###########################################################

source("R/01-helper_functions.R")
source("R/02-data_import.R")

## Import streets 

streets <- 
  getbb("montreal") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"),streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  select(osm_id, name, geometry)

streets <- 
  streets %>% st_join(montreal["geometry"],
                      join = st_within, left = FALSE)

## Mapping

tm_shape(montreal) +
  tm_borders(lwd = 1) +
  tm_shape(streets) +
  tm_lines(col = "grey", alpha = 0.5) +
  tm_shape(multiproperty) +
  tm_dots(col = "brown1",
          scale = 4/3,
          alpha = 0.5,
          size = "LTM_Revenue",
          size.lim = c(1, 10000),
          legend.show = F,
          legend.size.show = F) +
  tm_compass()