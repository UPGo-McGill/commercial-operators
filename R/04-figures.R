### 4 - MONTREAL ANALYSIS MAPS AND GRAPHS #############################################

source("R/01-helper_functions.R")
source("R/02-data_import.R")
source("R/03-analysis.R")
load("Data/Montreal_data.Rdata")

## Import streets ##################################################################### 

montreal <- 
  get_census(
    dataset = "CA16",
    regions = list(CMA = "24462"),
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)

CTs <- 
  get_census(
    dataset = "CA16",
    regions = list(CMA = "24462"),
    vectors = c("v_CA16_406", "v_CA16_5792", "v_CA16_5801", "v_CA16_5804", "v_CA16_5807", "v_CA16_405"),
    level = "CT",
    geo_format = "sf") %>% 
  st_transform(32618)

names(CTs)[15] <- "density"
names(CTs)[16] <- "occupied_dwellings"
names(CTs)[17] <- "commute_total"
names(CTs)[18] <- "commute_transit"
names(CTs)[19] <- "commute_walk"
names(CTs)[20] <- "commute_bike"

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

### Figure 1 - Map: Spatial distribution of listings at CMA extent ####################

listings_mtl <- 
  filter(property, created <= "2019-07-01", scraped >= "2019-07-01", housing == TRUE) %>% 
  strr_as_sf(32618) %>% 
  strr_raffle(CTs, GeoUID, Dwellings) %>% 
  st_drop_geometry() %>% 
  count(GeoUID)

CTs <- 
  CTs %>% 
  right_join(listings_mtl) %>% 
  select(GeoUID, dwellings = Dwellings, listings = n, geometry, density, commute_total, commute_transit,
         commute_walk, commute_bike, occupied_dwellings) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 5)

font_import()

boroughs <- st_read("Data/boroughs", "LIMADMIN")
boroughs <- boroughs %>% 
  st_transform(32618)

boroughs_filtered <- boroughs %>% 
  filter(NOM == "Le Plateau-Mont-Royal" | NOM == "Ville-Marie") %>% 
  st_transform(32618)

Figure1 <-
  CTs %>% 
  ggplot() +
  geom_sf(
    aes(fill = listings/dwellings),
    lwd = 0,
    colour = "white"
    ) +
  scale_fill_gradientn(
    colors = c("#455C7B", "#AC6C82", "#FFBC67"),
    na.value = "grey80",
    limits = c(0, 0.1),
    oob = scales::squish,
    labels = scales::percent,
    name = "Active STRs as share of total dwellings") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.justification = c(0, 1),
   legend.position = c(0, .95)) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 12),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("Output/Figure1.png", plot = Figure1, width = 6, 
       height = 5, units = "in")

### Figure 2 - Map: Density by census tract at Island extent ##########################

Figure2 <- 
  CTs %>% 
  ggplot() +
  geom_sf(
    aes(fill = density),
    lwd = 0,
    colour = "white"
  ) +
  scale_fill_gradientn(
    colors = c("#455C7B", "#AC6C82", "#FFBC67"),
    na.value = "grey80",
    limits = c(0, 15000),
    oob = scales::squish,
    labels = ,
    name = "Population density per square kilometre") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, .95)) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 12),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("Output/Figure2.png", plot = Figure2, width = 6, 
       height = 5, units = "in")

### Figure 3 - Map: Transit ridership by census tract at Island extent ############

metro <- st_read("Data/stm_sig", "stm_lignes_sig")
metro <- metro %>% 
  filter(route_name == "verte"| route_name == "orange" | route_name == "jaune" | route_name == "bleue") %>% 
  st_transform(32618)

Figure3 <- 
  CTs %>% 
  ggplot() +
  geom_sf(
    aes(fill = (commute_transit + commute_walk + commute_bike) / commute_total),
    lwd = 0,
    colour = "white"
  ) +
  scale_fill_gradientn(
    colors = c("#455C7B", "#AC6C82", "#FFBC67"),
    na.value = "grey80",
    limits = c(0, 0.75),
    oob = scales::squish,
    labels = scales::percent,
    name = "Active transport commuters per total commuter population") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, .95)) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 11),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10)) +
  geom_sf(
    data = metro,
    aes(colour = route_name),
    colour = c("forestgreen", "forestgreen", "darkorange2", "darkorange2", "darkorange2", "darkorange2", 
               "goldenrod1", "goldenrod1", "dodgerblue", "dodgerblue"),
    lwd = 1.5)

ggsave("Output/Figure3.png", plot = Figure3, width = 6, 
       height = 5, units = "in")
  
### Figure 4 - Map: Vacancy rates by census tract at CMA extent #######################

CMHC <- read_sf("Data/CMHC/CMHC_NBHD_2016-mercWGS84.shp")

CMHC <- 
  CMHC %>% 
  filter(METCODE == "1060") %>%
  select(-OBJECTID, -NBHDNAME_F, -NBHDNAME_1, -NBHDNAME_L, -NBHDNAME_E,
         -NAME_FR, -GEO_LAYER_, -SHAPE_Leng, -SHAPE_Area) %>% 
  st_transform(32618)

vacancy_nbhd <- read.csv("Data/CMHC/vacancy_nbhd.csv", colClasses = "character")

vacancy_nbhd$Total <- as.numeric(vacancy_nbhd$Total)

names(vacancy_nbhd)[1] <- "NBHDCODE"

CMHC <- 
  left_join(CMHC, vacancy_nbhd, by = "NBHDCODE")

CMHC <- 
  CTs %>% 
  st_join(CMHC, join = st_intersects, left = T)

CMHC <- 
  CMHC %>% 
  mutate(vacancy_rate = Total / 100)

Figure4 <- 
  CMHC %>% 
  ggplot() +
  geom_sf(
    aes(fill = vacancy_rate),
    lwd = 0,
    colour = "white"
  ) +
  scale_fill_gradientn(
    colors = c("#FFBC67", "#AC6C82", "#455C7B"),
    na.value = "grey80",
    limits = c(0, 0.03),
    oob = scales::squish,
    labels = scales::percent,
    name = "Vacancy rate") +
  theme_void() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, .95)) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 12),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("Output/Figure4.png", plot = Figure4, width = 6, 
       height = 5, units = "in")


### Figure 5 - Map: Spatial distribution of listings, 2015-2019 #######################

property_in_montreal <-
  property %>% 
  strr_as_sf(32618) %>% 
  select(-revenue) %>% 
  st_intersection(st_buffer(montreal, 250))

property_2016 <- 
  daily %>% 
  filter(status == "R", date >= "2016-01-01", date <= "2016-12-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_montreal, housing == TRUE), .) %>% 
  mutate(Year = "2016") %>% 
  filter(revenue > 0)

property_2017 <- 
  daily %>% 
  filter(status == "R", date >= "2017-01-01", date <= "2017-12-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_montreal, housing == TRUE), .) %>% 
  mutate(Year = "2017") %>% 
  filter(revenue > 0)

property_2018 <- 
  daily %>% 
  filter(status == "R", date >= "2018-01-01", date <= "2018-12-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_montreal, housing == TRUE), .) %>% 
  mutate(Year = "2018") %>% 
  filter(revenue > 0)

property_2019 <- 
  daily %>% 
  filter(status == "R", date >= start_date, date <= end_date) %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_montreal, housing == TRUE), .) %>% 
  mutate(Year = "2019") %>% 
  filter(revenue > 0)

Figure5 <- 
  rbind(property_2016, property_2017, property_2018, property_2019) %>%
  ggplot() +
  geom_sf(data = streets, colour = alpha("grey", 0.5)) +
  geom_sf(aes(size = revenue, colour = listing_type), alpha = 0.2, 
          show.legend = "point") +
  facet_wrap(vars(Year), nrow = 3) +
  scale_colour_manual(name = "Listing type",
                      values = c("#455C7B", "#AC6C82", "#FFBC67")) +
  scale_size_continuous(name = "Annual revenue",
                        breaks = c(20000, 40000, 60000, 80000, 100000),
                        labels = c("$20,000", "$40,000", "$60,000", "$80,000",
                                   "$100,000")) +
  guides(size = guide_legend(nrow = 3, byrow = TRUE),
         colour = guide_legend(
           override.aes = list(fill = c("#455C7B", "#AC6C82", "#FFBC67"), 
                               alpha = 1), nrow = 3, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(10, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank()) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 10),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("output/Figure5.png", plot = Figure5, width = 8, height = 9, units = "in")

### Figure 6 - Graph: Growth of active listings since 2015 ############################

Figure6 <-
  active_listings %>% 
  ggplot() +
  geom_ribbon(
    aes(xmin = as.Date("2019-05-01", "%Y-%m-%d"), 
        xmax = as.Date("2019-08-31",  "%Y-%m-%d"),
        y = n),
    fill = "#685C79",
    alpha = 0.5) +
  geom_ribbon(
    aes(xmin = as.Date("2018-05-01", "%Y-%m-%d"), 
        xmax = as.Date("2018-08-31",  "%Y-%m-%d"),
        y = n),
    fill = "#685C79",
    alpha = 0.5) +
  geom_ribbon(
    aes(xmin = as.Date("2017-05-01", "%Y-%m-%d"), 
        xmax = as.Date("2017-08-31",  "%Y-%m-%d"),
        y = n),
    fill = "#685C79",
    alpha = 0.5) +
  geom_ribbon(
    aes(xmin = as.Date("2016-05-01", "%Y-%m-%d"), 
        xmax = as.Date("2016-08-31",  "%Y-%m-%d"),
        y = n),
    fill = "#685C79",
    alpha = 0.5) +
  geom_line(aes(date, n), colour = "#685C79", size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  theme_minimal() +
  scale_x_date(name = NULL) +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 10),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("output/Figure6.png", plot = Figure6, width = 8, 
       height = 5, units = "in")


### Figure 8 - Graph: Host revenue distribution in top percentiles ####################

Figure8 <-
  daily %>%
  filter(housing == TRUE, date >= start_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value), stat = "identity", fill = "#DA727E") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  theme(text = element_text(family = "Segoe UI Semilight"),
        legend.title = element_text(family = "Segoe UI Black", size = 10),
        legend.text = element_text(family = "Segoe UI Semilight", size = 10))

ggsave("output/Figure8.png", plot = Figure8, width = 8, height = 4, 
       units = "in")

### Figure 9 - Graph: Growth of FREHs and FREH revenues since 2015 ####################

ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  filter(date >= "2016-01-01") %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

Figure9 <- 
  ML_summary %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL) +
  scale_colour_manual(values = c("#FFBC67", "#455C7B")) +
  theme(legend.position = "bottom")

ggsave("output/Figure9.png", plot = Figure9, width = 8, height = 7, 
       units = "in")

