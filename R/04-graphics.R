### 4 - MONTREAL ANALYSIS MAPS AND GRAPHS #############################################

source("R/01-helper_functions.R")
source("R/02-data_import.R")
source("R/03-analysis.R")
load("Data/Montreal_data.Rdata")

### Figure 1 - Map: Spatial distribution of listings at CMA extent ####################
CTs <- 
  get_census(
    dataset = "CA16",
    regions = list(CMA = "24462"),
    level = "CT",
    geo_format = "sf") %>% 
  st_transform(32618)

CTs %>% 
  ggplot() +
  geom_sf()

listings_mtl <- 
  filter(property, created <= "2019-07-01", scraped >= "2019-07-01", housing == TRUE) %>% 
  strr_as_sf(32618) %>% 
  strr_raffle(CTs, GeoUID, Dwellings) %>% 
  st_drop_geometry() %>% 
  count(GeoUID)

CTs <- 
  CTs %>% 
  left_join(listings_mtl) %>% 
  select(GeoUID, dwellings = Dwellings, listings = n, geometry) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 5)

Figure1 <- 
  CTs %>% 
  ggplot() +
  geom_sf(
    aes(fill = listings/dwellings),
    lwd = 0,
    colour = "white"
    ) +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268")
  )

ggsave("Output/Figure1.png", plot = Figure1, width = 8, 
       height = 5, units = "in")
  
### Figure 2 - Map: Vacancy rates by census tract at CMA extent #######################

st_read(dsn = system.file("Data/limadmin-shp"), crs = 32618)
st_read("Data/stm-sig/stm_lignes_sig")
vacancy <- read.csv("Data/vacancy.csv")

# FUCK THIS

### Figure 3 - Map: Spatial distribution of listings, 2016-2019 #######################

property_in_montreal <-
  property %>% 
  select(-revenue) %>% 
  st_intersection(st_buffer(HRM, 250))

property_2016 <- 
  daily %>% 
  filter(status == "R", date >= "2015-09-01", date <= "2016-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2016") %>% 
  filter(revenue > 0)

property_2017 <- 
  daily %>% 
  filter(status == "R", date >= "2016-09-01", date <= "2017-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2017") %>% 
  filter(revenue > 0)

property_2018 <- 
  daily %>% 
  filter(status == "R", date >= "2017-09-01", date <= "2018-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2018") %>% 
  filter(revenue > 0)

property_2019 <- 
  daily %>% 
  filter(status == "R", date >= start_date, date <= end_date) %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2019") %>% 
  filter(revenue > 0)

map <- 
  rbind(property_2016, property_2017, property_2018, property_2019) %>%
  ggplot() +
  geom_sf(data = HRM_streets, colour = alpha("grey", 0.5)) +
  geom_sf(aes(size = revenue, colour = listing_type), alpha = 0.2, 
          show.legend = "point") +
  facet_wrap(vars(Year), nrow = 2) +
  scale_colour_manual(name = "Listing type",
                      values = c("#4295A8", "#B4656F", "#C7F2A7")) +
  scale_size_continuous(name = "Annual revenue",
                        breaks = c(20000, 40000, 60000, 80000, 100000),
                        labels = c("$20,000", "$40,000", "$60,000", "$80,000",
                                   "$100,000"),
                        range = c(0.05, 2.5)) +
  guides(size = guide_legend(nrow = 3, byrow = TRUE),
         colour = guide_legend(
           override.aes = list(fill = c("#4295A8", "#B4656F", "#C7F2A7"), 
                               alpha = 1), nrow = 3, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(10, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank())

### Figure 4 - Graph: Growth of active listings since 2015 ############################


### Figure 5 - Graph: Listing growth by listing type since 2015 #######################


### Figure 6 - Graph: Host revenue distribution in top percentiles ####################


### Figure 7 - Graph: Growth of FREHs and FREH revenues since 2015 ####################


